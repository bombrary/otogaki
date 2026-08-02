import * as Tone from "tone";
import { DrumMachine, Soundfont } from "smplr";

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

function drumSampleNames() {
  const player = players["drumKit"];
  if (!player) return [];
  if (Array.isArray(player.sampleNames)) return player.sampleNames;
  if (typeof player.getSampleNames === "function") return player.getSampleNames();
  return [];
}

function resolveDrumSample(pitch) {
  if (drumSampleCache[pitch] !== undefined) return drumSampleCache[pitch];
  const names = drumSampleNames();
  const candidates = DRUM_ROLES[pitch] || [];
  let found = null;
  for (const cand of candidates) {
    found = names.find((n) => n.toLowerCase().includes(cand)) || null;
    if (found) break;
  }
  drumSampleCache[pitch] = found;
  if (!found) {
    console.warn("[audio] ピッチに対応するドラムサンプルがない:", pitch, "候補:", names);
  }
  return found;
}

export function setElmSender(fn) {
  sendToElm = fn;
}

function send(msg) {
  if (sendToElm) sendToElm(msg);
}

export async function ensureAudio() {
  if (!started) {
    await Tone.start();
    synth = new Tone.PolySynth(Tone.Synth).toDestination();
    synth.maxPolyphony = 64;
    started = true;
    send({ tag: "audioReady", payload: {} });
  }
}

function loaderFor(name, ctx) {
  switch (name) {
    case "piano":
      return new Soundfont(ctx, { instrument: "acoustic_grand_piano" });
    case "acousticGuitar":
      return new Soundfont(ctx, { instrument: "acoustic_guitar_nylon" });
    case "electricBass":
      // MusyngKite の electric_bass_finger は高音域でオクターブ落ちするので FluidR3_GM を使う
      return new Soundfont(ctx, { instrument: "electric_bass_finger", kit: "FluidR3_GM" });
    case "drumKit":
      return new DrumMachine(ctx, { instrument: "TR-808" });
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

export function handleCommand(msg) {
  switch (msg.tag) {
    case "play":
      play(msg.payload);
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
  let endTicks = events.reduce(
    (acc, e) => Math.max(acc, e.ticks + e.durationTicks),
    0
  );
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

  t.start(undefined, `${p.startTicks}i`);
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
