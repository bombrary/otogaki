import * as Tone from "tone";
import { DrumMachine, ElectricPiano, Mallet, Mellotron, Smolken, Soundfont, SplendidGrandPiano } from "smplr";

// Elm 側 Codec.Performance.metronomeTrackId と一致させる。自然停止の終端計算からメトロノームの
// クリックを除外するために使う（timeline 末尾の余白小節に食い込むのを防ぐ）。
const METRONOME_TRACK_ID = -2;

let started = false;
let synth = null;
let part = null;
let sendToElm = null;
const players = {};
const loadPromises = {};
let muteMap = {};
let volumeMap = {};
let currentPpq = 480;
let refBuffer = null;
let refPlayer = null;
let refMeta = { offsetMs: 0, volume: 80, muted: false };
let stopEventId = null;
// play()/stopPlayback() のたびに進む世代カウンタ。ストール検知タイマーが古い再生を誤検知しないためのガード。
let playGeneration = 0;

export async function loadRefAudio(file) {
  await ensureAudio();
  const arrayBuffer = await file.arrayBuffer();
  const ctx = Tone.getContext().rawContext;
  refBuffer = await ctx.decodeAudioData(arrayBuffer);
  // ピーク数は最大 6000 に押さえる（長い曲は自動的に粗く）
  const peakDt = Math.max(0.02, refBuffer.duration / 6000);
  send({
    tag: "refAudioReady",
    payload: {
      name: file.name,
      peaks: computePeaks(refBuffer, peakDt),
      peakDt: peakDt,
      duration: refBuffer.duration,
    },
  });
}

// 波形表示用に dt 秒ごとのピーク振幅（0..1）を計算する
function computePeaks(buffer, dt) {
  const sr = buffer.sampleRate;
  const win = Math.max(1, Math.floor(sr * dt));
  const ch0 = buffer.getChannelData(0);
  const ch1 = buffer.numberOfChannels > 1 ? buffer.getChannelData(1) : null;
  const n = Math.ceil(ch0.length / win);
  const peaks = new Array(n);
  for (let i = 0; i < n; i++) {
    let m = 0;
    const end = Math.min(ch0.length, (i + 1) * win);
    for (let j = i * win; j < end; j += 8) {
      const a = Math.abs(ch0[j]);
      if (a > m) m = a;
      if (ch1) {
        const b = Math.abs(ch1[j]);
        if (b > m) m = b;
      }
    }
    peaks[i] = Math.round(m * 100) / 100;
  }
  return peaks;
}

// 参考オーディオを transport に同期して張り直す。
// offsetMs 正: オーディオのその位置を小節1に合わせる / 負: オーディオを遅らせる
function setupRefPlayer(meta) {
  if (meta) refMeta = meta;
  if (refPlayer) {
    refPlayer.unsync();
    refPlayer.dispose();
    refPlayer = null;
  }
  if (!refBuffer || refMeta.muted) return;
  refPlayer = new Tone.Player(refBuffer).toDestination();
  refPlayer.volume.value = Tone.gainToDb(Math.max(0.001, (refMeta.volume ?? 80) / 100));
  const offSec = (refMeta.offsetMs || 0) / 1000;
  if (offSec >= 0) {
    refPlayer.sync().start(0, offSec);
  } else {
    refPlayer.sync().start(-offSec, 0);
  }
}

// GM ピッチ -> 役割候補語（キットの実サンプル名とあいまい一致させる）
const DRUM_ROLES = {
  35: ["kick", "bd"],
  36: ["kick", "bd"],
  37: ["rim", "stick"],
  38: ["snare", "sd"],
  39: ["clap"],
  40: ["snare", "sd"],
  41: ["tom-low", "low-tom", "lt", "tom"],
  42: ["hihat-close", "closed", "chh", "hh", "hihat", "hat"],
  43: ["tom-low", "low-tom", "lt", "tom"],
  44: ["hihat-close", "closed", "chh", "hihat", "hat"],
  45: ["tom-mid", "mid-tom", "mt", "tom"],
  46: ["hihat-open", "open", "ohh", "hihat", "hat"],
  47: ["tom-mid", "mid-tom", "mt", "tom"],
  48: ["tom-hi", "hi-tom", "high-tom", "ht", "tom"],
  49: ["crash", "cymbal", "cy"],
  50: ["tom-hi", "hi-tom", "ht", "tom"],
  51: ["ride", "cymbal", "cy"],
  54: ["tamb", "shaker", "maraca"],
  56: ["cowbell", "cow", "cb"],
  57: ["crash", "cymbal", "cy"],
  59: ["ride", "cymbal", "cy"],
};

let drumSampleCache = {};

function drumSampleNamesFor(player) {
  if (!player) return [];
  if (Array.isArray(player.sampleNames)) return player.sampleNames;
  if (typeof player.getSampleNames === "function") return player.getSampleNames();
  return [];
}

function resolveDrumSampleForPlayer(player, pitch, cache) {
  if (cache[pitch] !== undefined) return cache[pitch];
  const names = drumSampleNamesFor(player);
  const candidates = DRUM_ROLES[pitch] || [];
  let found = null;
  for (const cand of candidates) {
    found = names.find((n) => n.toLowerCase().includes(cand)) || null;
    if (found) break;
  }
  cache[pitch] = found;
  if (!found) {
    console.warn("[audio] ピッチに対応するドラムサンプルがない:", pitch, "候補:", names);
  }
  return found;
}

function drumSampleNames() {
  return drumSampleNamesFor(players["drumKit"]);
}

function resolveDrumSample(pitch) {
  return resolveDrumSampleForPlayer(players["drumKit"], pitch, drumSampleCache);
}

export function setElmSender(fn) {
  sendToElm = fn;
}

function send(msg) {
  if (sendToElm) sendToElm(msg);
}

// AudioContext が running かどうかを毎回確認し、中断されていれば復帰を試みる。
// iOS は「タブ切替・画面ロック・他アプリの音声」等で AudioContext を suspended/interrupted に落とし、
// 自動では戻らない。戻り値は「呼び出し元がこのまま音を鳴らしてよいか」。
export async function ensureAudio() {
  if (!started) {
    await Tone.start();
    synth = new Tone.PolySynth(Tone.Synth).toDestination();
    synth.maxPolyphony = 64;
    started = true;
    bindContextStatechange();
    send({ tag: "audioReady", payload: {} });
    return true;
  }

  const ctx = Tone.getContext().rawContext;
  if (ctx.state === "running") return true;

  // resume() の呼び出し開始は同期的に行う必要がある（iOS の user gesture 判定はコールスタックで見る）。
  // ここは async 関数の先頭からすぐ呼んでいるので、呼び出し元が pointerdown/click ハンドラの
  // 同期スタック内から ensureAudio を呼ぶ限りは gesture 内に収まる。
  const resumed = await Promise.race([
    ctx.resume().then(() => ctx.state === "running"),
    new Promise((resolve) => setTimeout(() => resolve(false), 1500)),
  ]).catch(() => false);

  if (resumed) return true;

  notifyAudioUnavailable();
  return false;
}

// AudioContext が running でなくなった瞬間（呼び出し起点を問わず）に一度だけ登録する。
let statechangeBound = false;
function bindContextStatechange() {
  if (statechangeBound) return;
  statechangeBound = true;
  const ctx = Tone.getContext().rawContext;
  ctx.addEventListener("statechange", () => {
    if (ctx.state !== "running") notifyAudioUnavailable();
  });
}

// 音声が使えない（中断されたまま復帰できなかった／自動検知で落ちた）ことを Elm に伝える。
export function notifyAudioUnavailable() {
  send({ tag: "audioSuspended", payload: {} });
}

// 開発時のデバッグ用。DevTools から AudioContext を直接触って中断状態を決定論的に再現する。
export function debugRawContext() {
  return Tone.getContext().rawContext;
}

function loaderFor(name, ctx, extraOpts = {}) {
  switch (name) {
    case "piano":
      // GM から差し替え。実録スタインウェイ（生音に近い）
      return new SplendidGrandPiano(ctx, extraOpts);
    case "electricPiano":
      return new ElectricPiano(ctx, { instrument: "WurlitzerEP200", ...extraOpts });
    case "organ":
      return new Soundfont(ctx, { instrument: "drawbar_organ", ...extraOpts });
    case "acousticGuitar":
      return new Soundfont(ctx, { instrument: "acoustic_guitar_nylon", ...extraOpts });
    case "steelGuitar":
      return new Soundfont(ctx, { instrument: "acoustic_guitar_steel", ...extraOpts });
    case "electricGuitarClean":
      return new Soundfont(ctx, { instrument: "electric_guitar_clean", ...extraOpts });
    case "electricGuitarJazz":
      return new Soundfont(ctx, { instrument: "electric_guitar_jazz", ...extraOpts });
    case "electricBass":
      // MusyngKite の electric_bass_finger は高音域でオクターブ落ちするので FluidR3_GM を使う
      return new Soundfont(ctx, { instrument: "electric_bass_finger", kit: "FluidR3_GM", ...extraOpts });
    case "uprightBass":
      return new Smolken(ctx, { instrument: "Pizzicato", ...extraOpts });
    case "vibraphone":
      return new Mallet(ctx, { instrument: "Vibraphone - Hard Mallets", ...extraOpts });
    case "strings":
      return new Soundfont(ctx, { instrument: "string_ensemble_1", ...extraOpts });
    case "mellotron":
      return new Mellotron(ctx, { instrument: "MKII VIOLINS", ...extraOpts });
    case "drumKit":
      return new DrumMachine(ctx, { instrument: "TR-808", ...extraOpts });
    default:
      return null;
  }
}

function loadInstrument(name) {
  if (name === "synthLead") return Promise.resolve();
  if (loadPromises[name]) return loadPromises[name];
  const ctx = Tone.getContext().rawContext;
  const inst = loaderFor(name, ctx);
  if (!inst) return Promise.resolve();
  players[name] = inst;
  loadPromises[name] = inst.load
    .then(() => {
      if (name === "drumKit") {
        drumSampleCache = {};
        console.log("[audio] ドラムサンプル一覧:", drumSampleNames());
      }
      send({ tag: "instrumentLoaded", payload: { instrument: name } });
    })
    .catch((err) => {
      console.error("[audio] instrument load failed:", name, err);
      delete players[name];
      delete loadPromises[name];
      send({ tag: "instrumentLoadFailed", payload: { instrument: name } });
    });
  return loadPromises[name];
}

function triggerEvent(e, time, durSec) {
  if (muteMap[e.trackId]) return;
  const vol = volumeMap[e.trackId] ?? 100;
  const vel = Math.round((e.velocity * vol) / 100);
  if (vel <= 0) return;
  const player = players[e.instrument];
  if (e.instrument === "synthLead" || !player) {
    synth.triggerAttackRelease(
      Tone.Frequency(e.pitch, "midi"),
      durSec,
      time,
      vel / 127
    );
    return;
  }
  if (e.instrument === "drumKit") {
    const sample = resolveDrumSample(e.pitch);
    if (sample) {
      // duration を渡さずワンショットで鳴らし切る（クラッシュ等が途切れないように）
      player.start({ note: sample, time: time, velocity: vel });
    }
    return;
  }
  player.start({
    note: e.pitch,
    time: time,
    duration: durSec,
    velocity: vel,
  });
}

function encodeWavPCM16(audioBuffer) {
  const numChannels = audioBuffer.numberOfChannels;
  const sampleRate = audioBuffer.sampleRate;
  const numFrames = audioBuffer.length;
  const bytesPerSample = 2;
  const blockAlign = numChannels * bytesPerSample;
  const dataSize = numFrames * blockAlign;
  const buffer = new ArrayBuffer(44 + dataSize);
  const view = new DataView(buffer);

  function writeString(offset, str) {
    for (let i = 0; i < str.length; i++) view.setUint8(offset + i, str.charCodeAt(i));
  }

  writeString(0, "RIFF");
  view.setUint32(4, 36 + dataSize, true);
  writeString(8, "WAVE");
  writeString(12, "fmt ");
  view.setUint32(16, 16, true);
  view.setUint16(20, 1, true);
  view.setUint16(22, numChannels, true);
  view.setUint32(24, sampleRate, true);
  view.setUint32(28, sampleRate * blockAlign, true);
  view.setUint16(32, blockAlign, true);
  view.setUint16(34, bytesPerSample * 8, true);
  writeString(36, "data");
  view.setUint32(40, dataSize, true);

  const channelData = [];
  for (let ch = 0; ch < numChannels; ch++) channelData.push(audioBuffer.getChannelData(ch));

  let offset = 44;
  for (let i = 0; i < numFrames; i++) {
    for (let ch = 0; ch < numChannels; ch++) {
      const sample = Math.max(-1, Math.min(1, channelData[ch][i]));
      const intSample = sample < 0 ? sample * 0x8000 : sample * 0x7fff;
      view.setInt16(offset, intSample, true);
      offset += 2;
    }
  }

  return new Blob([buffer], { type: "audio/wav" });
}

function downloadBlob(blob, fileName) {
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = fileName;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
}

// 書き出し区間のイベント列と長さを求める。loop があればその範囲だけにフィルタし、チックは区間始点基準の相対値にする。末尾には減衰・余音が切れないよう tailSec 秒のテールを足す。
function wavExportRange(payload) {
  const loop = payload.loop;
  const baseTicks = loop ? loop.startTicks : 0;
  const events = loop
    ? payload.events.filter((e) => e.ticks + e.durationTicks > loop.startTicks && e.ticks < loop.endTicks)
    : payload.events;
  const endTicks = loop
    ? loop.endTicks - loop.startTicks
    : payload.events.reduce((acc, e) => Math.max(acc, e.ticks + e.durationTicks), 0);
  const tailSec = 2;
  const durationSec = ticksToSeconds(endTicks, payload.bpm, payload.ppq) + tailSec;
  return { events, baseTicks, durationSec };
}

async function renderOffline(payload) {
  const sampleRate = Tone.getContext().rawContext.sampleRate;
  const { events, baseTicks, durationSec } = wavExportRange(payload);

  const muteByTrack = {};
  const volumeByTrack = {};
  payload.tracks.forEach((t) => {
    muteByTrack[t.id] = t.muted;
    volumeByTrack[t.id] = t.volume ?? 100;
  });

  const renderJob = Tone.Offline(async ({ rawContext }) => {
    const offlineSynth = new Tone.PolySynth(Tone.Synth).toDestination();
    offlineSynth.maxPolyphony = 64;

    const usedInstruments = [...new Set(events.map((e) => e.instrument))].filter((n) => n !== "synthLead");
    const offlinePlayers = {};
    const offlineDrumCache = {};
    await Promise.all(
      usedInstruments.map(async (name) => {
        const inst = loaderFor(name, rawContext, { disableScheduler: true });
        if (!inst) return;
        offlinePlayers[name] = inst;
        await inst.load;
      })
    );

    events.forEach((e) => {
      if (muteByTrack[e.trackId]) return;
      const vol = volumeByTrack[e.trackId] ?? 100;
      const vel = Math.round((e.velocity * vol) / 100);
      if (vel <= 0) return;
      const time = ticksToSeconds(e.ticks - baseTicks, payload.bpm, payload.ppq);
      if (time < 0) return;
      const dur = ticksToSeconds(e.durationTicks, payload.bpm, payload.ppq);
      const player = offlinePlayers[e.instrument];
      if (e.instrument === "synthLead" || !player) {
        offlineSynth.triggerAttackRelease(Tone.Frequency(e.pitch, "midi"), dur, time, vel / 127);
        return;
      }
      if (e.instrument === "drumKit") {
        const sample = resolveDrumSampleForPlayer(player, e.pitch, offlineDrumCache);
        if (sample) player.start({ note: sample, time, velocity: vel });
        return;
      }
      player.start({ note: e.pitch, time, duration: dur, velocity: vel });
    });
  }, durationSec, 2, sampleRate);

  const renderTimeout = new Promise((_, reject) =>
    setTimeout(() => reject(new Error("offline render timed out")), 30000)
  );

  const toneBuffer = await Promise.race([renderJob, renderTimeout]);
  const audioBuffer = toneBuffer.get();
  if (!audioBuffer) throw new Error("offline render produced no buffer");
  return audioBuffer;
}

// オフラインレンダリングが失敗した場合のフォールバック。実際に再生しながら ScriptProcessorNode でキャプチャするので、曲の長さ分実時間を要する。
async function renderRealtime(payload) {
  await ensureAudio();
  stopPlayback();
  applyTrackMeta(payload.tracks);
  currentPpq = payload.ppq;

  const { events, baseTicks, durationSec } = wavExportRange(payload);

  const usedInstruments = [...new Set(events.map((e) => e.instrument))];
  await Promise.all(usedInstruments.map(loadInstrument));

  const ctx = Tone.getContext().rawContext;
  const channels = 2;
  const bufferSize = 4096;
  const proc = ctx.createScriptProcessor(bufferSize, channels, channels);
  const chunks = [[], []];
  proc.onaudioprocess = (ev) => {
    for (let ch = 0; ch < channels; ch++) {
      chunks[ch].push(new Float32Array(ev.inputBuffer.getChannelData(ch)));
    }
  };
  const silentGain = ctx.createGain();
  silentGain.gain.value = 0;
  Tone.getDestination().connect(proc);
  proc.connect(silentGain);
  silentGain.connect(ctx.destination);

  const t = Tone.getTransport();
  t.PPQ = payload.ppq;
  t.bpm.value = payload.bpm;
  t.loop = false;
  part = buildPart(events, payload.ppq);
  t.start(undefined, `${baseTicks}i`);

  await new Promise((resolve) => setTimeout(resolve, durationSec * 1000));

  t.stop();
  t.cancel();
  if (part) {
    part.dispose();
    part = null;
  }
  Tone.getDestination().disconnect(proc);
  proc.disconnect();
  silentGain.disconnect();
  proc.onaudioprocess = null;

  const totalLength = chunks[0].reduce((acc, c) => acc + c.length, 0);
  const outBuffer = ctx.createBuffer(channels, totalLength, ctx.sampleRate);
  for (let ch = 0; ch < channels; ch++) {
    const data = outBuffer.getChannelData(ch);
    let offset = 0;
    for (const c of chunks[ch]) {
      data.set(c, offset);
      offset += c.length;
    }
  }
  return outBuffer;
}

async function renderWav(payload) {
  send({ tag: "wavRenderStarted", payload: {} });
  try {
    const audioBuffer = await renderOffline(payload);
    downloadBlob(encodeWavPCM16(audioBuffer), payload.fileName);
    send({ tag: "wavRenderDone", payload: {} });
  } catch (err) {
    console.warn("[audio] offline render failed, falling back to realtime capture:", err);
    try {
      const audioBuffer = await renderRealtime(payload);
      downloadBlob(encodeWavPCM16(audioBuffer), payload.fileName);
      send({ tag: "wavRenderDone", payload: {} });
    } catch (err2) {
      console.error("[audio] realtime capture also failed:", err2);
      send({ tag: "wavRenderFailed", payload: { message: String(err2) } });
    }
  }
}

export function handleCommand(msg) {
  switch (msg.tag) {
    case "play":
      play(msg.payload);
      break;
    case "renderWav":
      renderWav(msg.payload);
      break;
    case "stop":
      stopPlayback();
      break;
    case "setBpm":
      Tone.getTransport().bpm.value = msg.payload.bpm;
      break;
    case "seek":
      Tone.getTransport().ticks = msg.payload.ticks;
      break;
    case "setMute":
      muteMap[msg.payload.trackId] = msg.payload.muted;
      break;
    case "setVolume":
      volumeMap[msg.payload.trackId] = msg.payload.volume;
      break;
    case "updateEvents":
      updateEvents(msg.payload);
      break;
    case "loadInstruments":
      msg.payload.instruments.forEach(loadInstrument);
      break;
    case "previewNote":
      previewNote(msg.payload);
      break;
    default:
      console.warn("[audio] unknown command:", msg);
  }
}

function ticksToSeconds(ticks, bpm, ppq) {
  return (ticks / ppq) * (60 / bpm);
}

function applyTrackMeta(tracks) {
  muteMap = {};
  volumeMap = {};
  tracks.forEach((t) => {
    muteMap[t.id] = t.muted;
    volumeMap[t.id] = t.volume ?? 100;
  });
}

function buildPart(events, ppq) {
  const pairs = events.map((e) => [`${e.ticks}i`, e]);
  const p = new Tone.Part((time, e) => {
    const dur = ticksToSeconds(e.durationTicks, Tone.getTransport().bpm.value, ppq);
    triggerEvent(e, time, dur);
  }, pairs);
  p.start(0);
  return p;
}

// ループなし再生の自然停止をスケジュールし直す。
// MIDI イベントの終端だけでなく参考オーディオの実効長（offset 考慮）も含める。
// ノートやコードが未配置でも参考オーディオが途中で切れないように。
function scheduleNaturalStop(events, ppq) {
  const t = Tone.getTransport();
  if (stopEventId !== null) {
    t.clear(stopEventId);
    stopEventId = null;
  }
  if (t.loop) return;
  let endTicks = events
    .filter((e) => e.trackId !== METRONOME_TRACK_ID)
    .reduce((acc, e) => Math.max(acc, e.ticks + e.durationTicks), 0);
  if (refPlayer && refBuffer) {
    const offSec = (refMeta.offsetMs || 0) / 1000;
    const refEndSec = Math.max(0, refBuffer.duration - offSec);
    endTicks = Math.max(endTicks, Math.ceil(((refEndSec * t.bpm.value) / 60) * ppq));
  }
  stopEventId = t.scheduleOnce(() => {
    stopPlayback();
    send({ tag: "stopped", payload: {} });
  }, `${endTicks}i`);
}

async function play(p) {
  stopPlayback();

  applyTrackMeta(p.tracks);
  currentPpq = p.ppq;

  const usedInstruments = [...new Set(p.events.map((e) => e.instrument))];
  await Promise.all(usedInstruments.map(loadInstrument));

  const t = Tone.getTransport();
  t.PPQ = p.ppq;
  t.bpm.value = p.bpm;

  part = buildPart(p.events, p.ppq);
  setupRefPlayer(p.refAudio);

  if (p.loop) {
    t.setLoopPoints(`${p.loop.startTicks}i`, `${p.loop.endTicks}i`);
    t.loop = true;
  } else {
    t.loop = false;
  }
  scheduleNaturalStop(p.events, p.ppq);

  t.scheduleRepeat(() => {
    send({ tag: "playhead", payload: { ticks: Math.round(t.ticks) } });
  }, "16n");

  // Elm 側（Main.elm の startPlay）でも同様のクランプをしているが、片方の修正漏れで再発しないように JS 側でも守る。
  // ループなしで再生ヘッドが内容の終端以降にあると、scheduleNaturalStop の停止イベントが過去の時刻に積まれて発火しない。
  const contentEndTicks = p.events
    .filter((e) => e.trackId !== METRONOME_TRACK_ID)
    .reduce((acc, e) => Math.max(acc, e.ticks + e.durationTicks), 0);
  const effectiveStartTicks = !p.loop && p.startTicks >= contentEndTicks ? 0 : p.startTicks;

  t.start(undefined, `${effectiveStartTicks}i`);

  // AudioContext が中断されたままだと Transport のクロックが進まず、ボタンは反応しているのに音も再生ヘッドも動かない状態になる。
  // ensureAudio で捕らえ切れなかったケースの保険として、実際に進んだかを短時間後に確認する。
  const myGeneration = playGeneration;
  setTimeout(() => {
    if (playGeneration !== myGeneration) return;
    const ctx = Tone.getContext().rawContext;
    const stalled = ctx.state !== "running" || t.state !== "started" || t.ticks <= effectiveStartTicks;
    if (stalled) notifyAudioUnavailable();
  }, 300);
}

// 再生中にイベント列だけ差し替える（トランスポートは止めない）
function updateEvents(p) {
  applyTrackMeta(p.tracks);
  currentPpq = p.ppq;
  const usedInstruments = [...new Set(p.events.map((e) => e.instrument))];
  usedInstruments.forEach(loadInstrument);

  const t = Tone.getTransport();
  if (t.state !== "started") return;
  if (part) {
    part.dispose();
    part = null;
  }
  part = buildPart(p.events, p.ppq);
  setupRefPlayer(p.refAudio);
  // 再生中の編集で曲が伸びた場合に備えて停止位置も引き直す
  scheduleNaturalStop(p.events, p.ppq);
}

function stopPlayback() {
  playGeneration++;
  const t = Tone.getTransport();
  t.stop();
  t.cancel();
  stopEventId = null;
  t.loop = false;
  if (part) {
    part.dispose();
    part = null;
  }
  if (refPlayer) {
    refPlayer.unsync();
    refPlayer.dispose();
    refPlayer = null;
  }
  if (synth) synth.releaseAll();
  Object.values(players).forEach((player) => {
    if (player.stop) player.stop();
  });
}

function previewNote(payload) {
  const player = players[payload.instrument];
  const now = Tone.getContext().rawContext.currentTime;
  if (payload.instrument === "drumKit" && player) {
    const sample = resolveDrumSample(payload.pitch);
    if (sample) player.start({ note: sample, time: now, velocity: 100 });
    return;
  }
  if (player) {
    player.start({ note: payload.pitch, time: now, duration: 0.3, velocity: 100 });
    return;
  }
  synth.triggerAttackRelease(Tone.Frequency(payload.pitch, "midi"), "8n");
}
