let str =
  "<script lang=\"ts\">\n\
   import type { HtmlTag } from 'svelte/internal';\n\
   import type { GameConfig } from './shared';\n\n\
   import * as topicIcons from './topicicons.json'\n\
   import topicSpecial from './topicspecial'\n\n\
   export let room: string\n\n\
   export let connection: 'waiting' | 'connecting' | 'connected'\n\n\
   export let game_config: GameConfig\n\
   // export let state // loading, waiting, playing, paused.\n\
   // export let start_time\n\
   // export let topic\n\
   // export let meditate\n\
   // export let players\n\
   // export let rounds\n\
   // export let seconds_per_bead\n\
   // export let paused_progress\n\n\
   export let _active_sessions: number\n\
   export let _magister: true | null\n\
   export let _clock_offset: number\n\n\
   // let game_completed = false // Derived from other properties\n\n\
   let round_audio: HTMLAudioElement\n\
   let complete_audio: HTMLAudioElement\n\
   let topic_img: HTMLElement\n\
   let topic_text: HTMLElement\n\n\
   round_audio = new Audio()\n\
   round_audio.src = \"/lo-metal-tone.mp3\"\n\
   complete_audio = new Audio()\n\
   complete_audio.src = \"/hi-metal-tone.mp3\"\n\
   // round_audio.preload = 'auto'\n\
   \t// <audio bind:this={complete_audio} src=\"/hi-metal-tone.mp3\" \
   preload=\"auto\"><track kind=\"captions\"></audio>\n\n\n\
   let state: GameConfig['state']\n\
   $: state = game_config.state\n\n\
   $: console.log('Game configuration changed', game_config)\n\n\
   \t// export let state\n\n\
   const ARCHETOPICS = [\n\
  \  'Truth', 'Human', 'Energy', 'Beauty', 'Beginning', 'End', 'Birth', 'Death',\n\
  \  'Ego', 'Attention', 'Art', 'Empathy', 'Eutopia', 'Future', 'Game', 'Gift',\n\
  \  'History', 'Cosmos', 'Time', 'Life', 'Addiction', 'Paradox', 'Shadow', \
   'Society'\n\
   ]\n\n\n\n\
   let audio_works = true\n\n\
   function test_audio() {\n\
   \t// This ugly monstrosity brought to you by iOS Safari. \n\
   \t// This seems to be the only way to bless the audio\n\
   \t// objects to be able to play during the game. :/\n\
   \t\n\
   \t// let a = new Audio()\n\
   \t// a.volume = 0.1\n\
   \tconst round_src = round_audio.src\n\
   \tconst complete_src = complete_audio.src\n\
   \tround_audio.src = complete_audio.src = '/silence.mp3'\n\
   \t// a.play().then(\n\
   \tcomplete_audio.play()\n\
   \tround_audio.play().then(\n\
   \t\t() => {\n\
   \t\t\taudio_works = true\n\
   \t\t\tround_audio.src = round_src\n\
   \t\t\tcomplete_audio.src = complete_src\n\
   \t\t\tconsole.log('Audio works')\n\
   \t\t},\n\
   \t\t() => {\n\
   \t\t\taudio_works = false\n\
   \t\t\tround_audio.src = round_src\n\
   \t\t\tcomplete_audio.src = complete_src\n\
   \t\t\tconsole.log('Audio does not work')\n\
   \t\t}\n\
   \t)\n\
   }\n\
   function fix_audio() {\n\
   \tconsole.log('fixxx')\n\
   \ttest_audio()\n\
   }\n\
   setTimeout(test_audio, 0)\n\
   document.onclick = () => {\n\
   \tif (!audio_works) test_audio()\n\
   }\n\n\
   const fixed_rand = Math.random()\n\
   const randInt = (n: number) => Math.floor(fixed_rand * n)\n\
   function randItem<T>(arr: T[]) {return arr[randInt(arr.length)] }\n\n\
   $: {\n\
   \tif (topic_img && topic_text) {\n\
   \t\tconst topic = game_config.topic.toLocaleLowerCase()\n\
   \t\tconst svgContent = topicIcons[topic as keyof typeof topicIcons]\n\
   \t\tconst textContent = topicSpecial[topic as keyof typeof topicSpecial]\n\n\
   \t\tif (svgContent) {\n\
   \t\t\ttopic_img.innerHTML = svgContent\n\
   \t\t\ttopic_text.innerText = ''\n\
   \t\t} else if (textContent) {\n\
   \t\t\ttopic_img.innerHTML = ''\n\
   \t\t\ttopic_text.innerText = randItem(textContent)\n\
   \t\t} else {\n\
   \t\t\ttopic_img.innerHTML = ''\n\
   \t\t\ttopic_text.innerText = game_config.topic\n\
   \t\t}\n\
   \t}\n\
   }\n\n\
   // Could make configurable. Eh.\n\
   const MEDITATION_SECONDS = 60\n\n\
   interface GameStage {\n\
   \tlabel: string,\n\
   \ttype: 'waiting' | 'bead' | 'breath' | 'meditate' | 'contemplation' | \
   'complete',\n\
   \tduration: number,\n\
   \tno_sound?: true,\n\
   \tr?: number, p?: number,\n\
   \tid?: string\n\
   }\n\n\
   let game_stages: GameStage[] = []\n\
   $: {\n\
   \tgame_stages = [{\n\
   \t\tlabel: `${game_config.meditate ? 'Meditation' : 'Game'} starting...`,\n\
   \t\ttype: 'waiting',\n\
   \t\tduration: 3,\n\
   \t\tno_sound: true\n\
   \t}]\n\
   \tif (game_config.meditate) game_stages.push({\n\
   \t\tlabel: 'Meditate',\n\
   \t\ttype: 'meditate',\n\
   \t\tduration: MEDITATION_SECONDS,\n\
   \t})\n\
   \tfor (let r = 0; r < game_config.rounds; r++) {\n\
   \t\tfor (let p = 0; p < game_config.players; p++) {\n\
   \t\t\tif (game_config.seconds_between_bead && (r > 0 || p > 0)) \
   game_stages.push({\n\
   \t\t\t\tlabel: 'Breathe',\n\
   \t\t\t\tduration: game_config.seconds_between_bead,\n\
   \t\t\t\ttype: 'breath',\n\
   \t\t\t\tid: `b ${r} ${p}`\n\
   \t\t\t})\n\n\
   \t\t\tgame_stages.push({\n\
   \t\t\t\tlabel: '',\n\
   \t\t\t\t// label: game_config.players > 1 ? `Round ${r+1} player ${p+1}` : \
   `Round ${r+1}`,\n\
   \t\t\t\tduration: game_config.seconds_per_bead,\n\
   \t\t\t\ttype: 'bead', r, p,\n\
   \t\t\t\tid: `s ${r} ${p}`\n\
   \t\t\t})\n\
   \t\t}\n\
   \t}\n\n\
   \tif (game_config.contemplation) game_stages.push({\n\
   \t\tlabel: \"Contemplate the game's passing\",\n\
   \t\ttype: 'contemplation',\n\
   \t\tduration: MEDITATION_SECONDS,\n\
   \t})\n\n\n\
   \tconsole.log('game stages', game_stages, game_config.seconds_between_bead)\n\
   }\n\n\
   let total_game_length: number\n\
   $: total_game_length = game_stages.reduce((x, s) => x + s.duration, 0)\n\n\
   // Used for the overall game progress indicator.\n\
   let inner_game_stages: GameStage[]\n\
   $: inner_game_stages = game_stages.filter(s => s.type === 'breath' || \
   s.type === 'bead')\n\
   let inner_game_length: number\n\
   $: inner_game_length = inner_game_stages.reduce((x, s) => x + s.duration, \
   0)\n\n\
   // TODO: The protocol for these update methods doesn't use game_state \
   properly.\n\
   const update_state = async (patch: Record<string, string | number | boolean \
   | null>) => {\n\
   \tawait fetch(`${room}/configure`, {\n\
   \t\tmethod: 'POST',\n\
   \t\tmode: 'same-origin',\n\
   \t\theaders: {\n\
   \t\t\t'content-type': 'application/json',\n\
   \t\t},\n\
   \t\tbody: JSON.stringify(patch)\n\
   \t})\n\
   }\n\n\
   const upd = (k: string, v: string | number | boolean | null) => () => \
   update_state({[k]: v})\n\n\
   const config = (k: string): svelte.JSX.FormEventHandler<HTMLInputElement> \
   => (e) => {\n\
   \t// console.log('k', k, e.data, e.value, e.target.value, e.target.type)\n\
   \tconst target = e.target as HTMLInputElement\n\
   \tconst raw_value = target.value\n\
   \tconst value = target.type === 'number' ? ~~raw_value\n\
   \t\t: target.type === 'checkbox' ? target.checked\n\
   \t\t: raw_value\n\
   \tupdate_state({[k]: value})\n\
   }\n\n\
   const roundish = (x: number) => Math.round(x * 10) / 10\n\n\n\
   const waiting_stage: GameStage = { label: 'Waiting to start', type: \
   'waiting', duration: Infinity }\n\
   const complete_stage: GameStage = { label: 'Game complete', type: \
   'complete', duration: Infinity }\n\
   const get_current_stage = (offset_ms: number): {stage: GameStage, \
   stage_idx: number, offset_sec: number} => {\n\
   \tif (state === 'waiting') return {stage: waiting_stage, stage_idx: -1, \
   offset_sec: 0}\n\n\
   \tlet offset_sec = Math.round(offset_ms / 1000)\n\
   \tfor (let s = 0; s < game_stages.length; s++) {\n\
   \t\tlet stage = game_stages[s]\n\
   \t\tif (stage.duration > offset_sec) {\n\
   \t\t\treturn {stage, stage_idx: s, offset_sec}\n\
   \t\t}\n\
   \t\toffset_sec -= stage.duration\n\
   \t}\n\
   \treturn {\n\
   \t\tstage: complete_stage, stage_idx: game_stages.length, offset_sec\n\
   \t}\n\
   }\n\n\
   // Urgh kinda ugly storing state for both the index and stage itself. \
   Better to\n\
   // have one derive the other.\n\
   let current_stage: GameStage | null = null, current_stage_idx: number = -1, \
   offset_sec: number\n\
   $: console.log('current stage', current_stage)\n\
   // $: console.log('idx', current_stage_idx)\n\n\
   const tick = (play_audio: boolean) => {\n\
   \tconsole.log('tick')\n\
   \t// console.log('state', state, 'completed', state && state.complete)\n\n\
   \tconst time = state === 'playing' ? Date.now() + _clock_offset - \
   game_config.start_time\n\
   \t\t: state === 'paused' ? game_config.paused_progress!\n\
   \t\t: 0\n\
   \tconst {stage: new_stage, stage_idx: new_stage_idx, offset_sec: new_offs} \
   = get_current_stage(time)\n\
   \t// state_label = state.label\n\n\
   \toffset_sec = new_offs\n\
   \tif (new_stage !== current_stage) {\n\
   \t\tconsole.log('state changed', new_stage.label, new_stage.type === \
   'complete')\n\n\
   \t\t// This happens sometimes with other kinds of configuration changes -\n\
   \t\t// eg if a user enters or leaves the room, or the room is reconfigured.\n\
   \t\t// Only make a sound if the *stage* changes.\n\
   \t\tlet changed = current_stage == null || (new_stage.id ?? new_stage.type) \
   !== (current_stage.id ?? current_stage.type)\n\
   \t\t// console.log(new_stage, current_stage, changed)\n\n\
   \t\tcurrent_stage = new_stage\n\
   \t\tcurrent_stage_idx = new_stage_idx\n\
   \t\t// completed = new_game_state.complete\n\
   \t\t// if (!state.complete) round_audio.play()\n\n\
   \t\tif (play_audio && !new_stage.no_sound && changed) {\n\
   \t\t\tif (current_stage.type === 'complete' || current_stage.type === \
   'contemplation') complete_audio.play()\n\
   \t\t\telse round_audio.play()\n\
   \t\t}\n\
   \t}\n\
   }\n\n\
   let timer: number | null | any // Timeout?\n\
   $: {\n\
   \t// Sadly we can't use internal_state here because it generates a cyclic \
   dependancy.\n\
   \tlet completed = current_stage ? current_stage.type === 'complete' : false\n\
   \t// console.log('xx', state, timer, completed, current_stage)\n\n\
   \t// if (state !== 'loading') tick(false)\n\n\
   \tif (state === 'playing' && timer == null && !completed) {\n\
   \t\t// setTimeout needed to get around some weird race condition.\n\
   \t\t// There's probably better ways to structure this :/\n\
   \t\tsetTimeout(() => tick(false))\n\
   \t\ttimer = setInterval(() => {\n\
   \t\t\ttick(true)\n\
   \t\t}, 1000)\n\
   \t} else if ((completed || state !== 'playing') && timer != null) {\n\
   \t\tconsole.log('cancelled interval timer')\n\
   \t\tclearInterval(timer)\n\
   \t\ttimer = null\n\
   \t} else if (state === 'waiting' || state === 'paused') {\n\
   \t\tsetTimeout(() => tick(false))\n\
   \t}\n\
   }\n\n\
   let game_completed: boolean\n\
   $: {\n\
   \t// console.log('updating game_completed', current_stage)\n\
   \tgame_completed = (state !== 'playing' || current_stage == null) ? false\n\
   \t: (current_stage.type === 'complete')\n\
   }\n\n\
   let internal_state: GameConfig['state'] | 'completed'\n\
   $: internal_state = game_completed ? 'completed' : state\n\n\
   let bar_width = 0\n\
   $: bar_width = current_stage == null ? 0\n\
   \t: state === 'waiting' ? 0\n\
   \t: current_stage.type === 'complete' ? 100\n\
   \t: 100 * offset_sec / current_stage.duration\n\n\
   let stage_label: string\n\
   $: stage_label = internal_state === 'waiting' ? 'Waiting to start'\n\
   \t: current_stage == null ? 'unknown' : current_stage.label\n\n\n\
   const progress_class = (stage_idx: number, baseline_idx: number): 's-done' \
   | 's-active' | 's-waiting' => {\n\
   \tif (current_stage == null || baseline_idx < 0) return 's-waiting'\n\n\
   \treturn stage_idx < baseline_idx ? 's-done'\n\
   \t\t: stage_idx === baseline_idx ? 's-active'\n\
   \t\t: 's-waiting'\n\
   }\n\n\
   // This will get more complex in time. For now, pause the game to fiddle.\n\
   $: settings_disabled = state === 'playing'\n\n\
   let config_open = false\n\n\
   $: if (_magister === true) config_open = true\n\n\
   // The first user has the config open by default.\n\
   // $: if (_active_sessions === 1) config_open = true\n\n\
   // The magister box is fully visible once there's a critical mass of \
   players in the room\n\
   $: magister_opaque = _magister === true || _active_sessions >= 6\n\n\
   </script>\n\n\
   <svelte:head>\n\
   \t{#if _magister}\n\
   \t\t<style>\n\
   body {\n\
   \tbackground-color: var(--bg-highlight);\n\
   }\n\
   \t\t</style>\n\
   \t{/if}\n\
   </svelte:head>\n\n\
   <!-- <main class:magister={_magister}> -->\n\
   <main>\n\
   \t<!-- <audio bind:this={round_audio} src=\"/lo-metal-tone.mp3\" \
   preload=\"auto\" autoplay><track kind=\"captions\"></audio>\n\
   \t<audio bind:this={complete_audio} src=\"/hi-metal-tone.mp3\" \
   preload=\"auto\"><track kind=\"captions\"></audio> -->\n\n\
   \t{#if !audio_works}\n\
   \t\t<button id='fixaudio' on:click={fix_audio}>Audio muted. Click to \
   unmute</button>\n\
   \t{/if}\n\n\
   \t{#if internal_state === 'loading'}\n\
   \t\t<h1>Loading game state</h1>\n\
   \t{:else}\n\
   \t\t<!-- <h1>Glass Bead Game Timer</h1> -->\n\
   \t\t<!-- <h1>{topic}</h1> -->\n\n\
   \t\t<div id='topic'>\n\
   \t\t\t<div id='topicimg' bind:this={topic_img}></div>\n\
   \t\t\t<div id='topictext' bind:this={topic_text}></div>\n\
   \t\t</div>\n\n\
   \t\t<h1 id='stagelabel'>{stage_label}</h1>\n\
   \t\t<div id='progresscontainer'>\n\
   \t\t\t<div id='progress_time'>{((internal_state === 'playing' || \
   internal_state === 'paused') && current_stage) ? current_stage.duration - \
   offset_sec : ''}</div>\n\
   \t\t\t<div id='progress' style='width: {bar_width}%'></div>\n\
   \t\t</div>\n\n\
   \t\t<div id='gameprogress'>\n\
   \t\t\t{#each game_stages as s, i}\n\
   \t\t\t\t{#if s.type === 'bead' || s.type === 'breath'}\n\
   \t\t\t\t\t<span class={'prog-' + s.type + ' ' + progress_class(i, \
   current_stage_idx)} style='width: {100 * s.duration / \
   inner_game_length}%'></span>\n\
   \t\t\t\t{/if}\n\
   \t\t\t{/each}\n\
   \t\t</div>\n\n\
   \t\t{#if (_magister == null || _magister == true)}\n\
   \t\t\t{#if internal_state == 'waiting'}\n\
   \t\t\t\t<button on:click={upd('state', 'playing')}>Start</button>\n\
   \t\t\t{:else if internal_state == 'playing'}\n\
   \t\t\t\t<button on:click={upd('state', 'paused')}>Pause</button>\n\
   \t\t\t{:else if internal_state == 'paused'}\n\
   \t\t\t\t<button on:click={upd('state', 'playing')}>Resume</button>\n\
   \t\t\t{/if}\n\
   \t\t{/if}\n\n\
   \t\t<div style='height: 400px;'></div>\n\n\
   \t\t<details>\n\
   \t\t\t<!-- I'm not ready to delete these UI elements but we might not use \
   them -->\n\
   \t\t\t<summary>Info</summary>\n\n\
   \t\t\t<h1>{game_config.topic}</h1>\n\
   \t\t\t<h4>Room: <em>{room}</em> <a href=\"../..\">(leave)</a></h4>\n\n\
   \t\t\t<div>\n\
   \t\t\t\t{state === 'waiting' ? 'Waiting for the game to start'\n\
   \t\t\t\t: state === 'paused' ? 'GAME PAUSED'\n\
   \t\t\t\t: state === 'playing' ? 'Game in progress'\n\
   \t\t\t\t: ''}\n\
   \t\t\t</div>\n\
   \t\t\t{#if connection !== 'connected'}\n\
   \t\t\t\t<div>DISCONNECTED FROM GAME SERVER</div>\n\
   \t\t\t{:else}\n\
   \t\t\t\t{#if _active_sessions == 1}\n\
   \t\t\t\t\t<div>You are alone in the room</div>\n\
   \t\t\t\t{:else}\n\
   \t\t\t\t\t<div>{_active_sessions} players are in this room</div>\n\
   \t\t\t\t{/if}\n\
   \t\t\t{/if}\n\
   \t\t</details>\n\n\
   \t\t{#if _magister == null || _magister == true}\n\
   \t\t\t<details class='config' bind:open={config_open}>\n\
   \t\t\t\t<summary>Game controls</summary>\n\n\
   \t\t\t\t<p>\n\
   \t\t\t\t\t{#if _magister == null}\n\
   \t\t\t\t\t\tThis will effect all players. Will you borrow power? Will you \
   steal it?\n\
   \t\t\t\t\t{:else}\n\
   \t\t\t\t\t\tYou are master of the games. These controls are yours alone.\n\
   \t\t\t\t\t{/if}\n\
   \t\t\t\t</p>\n\n\
   \t\t\t\t{#if internal_state == 'waiting'}\n\
   \t\t\t\t\t<button on:click={upd('state', 'playing')}>Start</button>\n\
   \t\t\t\t{:else if internal_state == 'playing'}\n\
   \t\t\t\t\t<button on:click={upd('state', 'paused')}>Pause</button>\n\
   \t\t\t\t{:else if internal_state == 'paused'}\n\
   \t\t\t\t\t<button on:click={upd('state', 'playing')}>Resume</button>\n\
   \t\t\t\t{/if}\n\n\
   \t\t\t\t{#if internal_state == 'paused' || internal_state == 'completed' }\n\
   \t\t\t\t\t<button on:click={upd('state', 'waiting')}>Restart game</button>\n\
   \t\t\t\t{/if}\n\n\
   \t\t\t\t<label>\n\
   \t\t\t\t\t<span>Topic</span>\n\
   \t\t\t\t\t<input disabled={settings_disabled} type='text' \
   value={game_config.topic} on:input={config('topic')} list='archetopics' >\n\
   \t\t\t\t\t<datalist id='archetopics'>\n\
   \t\t\t\t\t\t{#each ARCHETOPICS as topic}\n\
   \t\t\t\t\t\t\t<option value={topic}>\n\
   \t\t\t\t\t\t{/each}\n\
   \t\t\t\t\t</datalist>\n\
   \t\t\t\t</label>\n\n\
   \t\t\t\t<label>\n\
   \t\t\t\t\t<span>Pre-game meditation</span>\n\
   \t\t\t\t\t<input disabled={settings_disabled} type='checkbox' \
   checked={game_config.meditate} on:input={config('meditate')} >\n\
   \t\t\t\t</label>\n\n\
   \t\t\t\t<label>\n\
   \t\t\t\t\t<span>Post game contemplation</span>\n\
   \t\t\t\t\t<input disabled={settings_disabled} type='checkbox' \
   checked={game_config.contemplation} on:input={config('contemplation')} >\n\
   \t\t\t\t</label>\n\n\
   \t\t\t\t<label>\n\
   \t\t\t\t\t<span>Number of players</span>\n\
   \t\t\t\t\t<input disabled={settings_disabled} type='number' \
   pattern='[0-9]*' value={game_config.players} on:input={config('players')} \
   min=1 max=12 >\n\
   \t\t\t\t</label>\n\n\
   \t\t\t\t<label>\n\
   \t\t\t\t\t<span>Number of rounds</span>\n\
   \t\t\t\t\t<input disabled={settings_disabled} type='number' \
   pattern='[0-9]*' value={game_config.rounds} on:input={config('rounds')} \
   min=1 max=20>\n\
   \t\t\t\t</label>\n\n\
   \t\t\t\t<label>\n\
   \t\t\t\t\t<span>Seconds per bead</span>\n\
   \t\t\t\t\t<input disabled={settings_disabled} type='number' \
   pattern='[0-9]*' value={game_config.seconds_per_bead} \
   on:input={config('seconds_per_bead')}>\n\
   \t\t\t\t</label>\n\n\
   \t\t\t\t<label>\n\
   \t\t\t\t\t<span>Seconds between beads</span>\n\
   \t\t\t\t\t<input disabled={settings_disabled} type='number' \
   pattern='[0-9]*' value={game_config.seconds_between_bead} \
   on:input={config('seconds_between_bead')}>\n\
   \t\t\t\t</label>\n\n\
   \t\t\t\t<div style='margin-top: 1em;'>\n\
   \t\t\t\t\t(Total game length: {roundish(\n\
   \t\t\t\t\t\tgame_stages.reduce((x, s) => x + s.duration, 0) / 60\n\
   \t\t\t\t\t)} minutes)\n\
   \t\t\t\t</div>\n\n\
   \t\t\t\t<div id='magister_box' class:magister_opaque>\n\
   \t\t\t\t\t{#if _magister == null}\n\
   \t\t\t\t\t\t<button on:click={upd('_magister', true)}>Assume the mantle of \
   Magister Ludi</button>\n\
   \t\t\t\t\t\t<p><i>Advanced - for large games</i></p>\n\
   \t\t\t\t\t\t<p>When present, the Magister Ludi (master of the games) has \
   exclusive control of the game.</p>\n\
   \t\t\t\t\t{:else if _magister == true}\n\
   \t\t\t\t\t\t<button on:click={upd('_magister', null)}>Abdicate Magister \
   Ludi status</button>\n\
   \t\t\t\t\t\t<p>You are the master of the games. You have exclusive control \
   over playing, pausing and configuring this game.</p>\n\
   \t\t\t\t\t\t<p>Do not close this browser window or you will be dethroned.</p>\n\
   \t\t\t\t\t{/if}\n\
   \t\t\t\t</div>\n\
   \t\t\t</details>\n\
   \t\t{:else}\n\
   \t\t\t<p class='config'>Magister Ludi is managing this game.</p>\n\
   \t\t{/if}\n\
   \t{/if}\n\
   </main>\n\n\
   <style>\n\n\
   main {\n\
   \t/* margin-bottom: 3em; */\n\
   \ttext-align: center;\n\
   }\n\n\
   #fixaudio {\n\
   \tz-index: 1;\n\
   \tcolor: var(--fg-color);\n\
   \tbackground-color: var(--bg-highlight);\n\
   \tposition: absolute;\n\
   \tbottom: 2px;\n\
   \twidth: 300px;\n\
   \tpadding: 0.5em 1em;\n\
   \tleft: 50%;\n\
   \ttransform: translateX(-50%);\n\
   \tfont-size: 130%;\n\
   }\n\n\
   #topicimg {\n\
   \twidth: 300px;\n\
   \tdisplay: inline-block;\n\
   }\n\
   #topictext:not(:empty) {\n\
   \tpadding: 3em 0 2em 0;\n\
   \tfont-size: 130%;\n\
   \tfont-style: italic;\n\
   }\n\n\
   /* .magister {\n\
   \tbackground-color: var(--bg-highlight);\n\
   } */\n\n\
   /* h1 {\n\
   \tmargin-top: 1em;\n\
   } */\n\n\
   #stagelabel:empty {\n\
   \theight: 1.2em;\n\
   }\n\n\
   #progresscontainer {\n\
   \t/* width: calc(100% - 50px); */\n\
   \tposition: relative;\n\
   \tmargin: 10px 25px;\n\
   \theight: 5em;\n\
   \tborder: 2px solid var(--fg-color);\n\
   \t/* margin-bottom: 0; */\n\
   }\n\n\
   #progress_time {\n\
   \tposition: absolute;\n\
   \t/* color: red; */\n\
   \t/* font-size: var(--bg-color); */\n\
   \tcolor: white;\n\
   \t/* color: white; */\n\
   \tfont-size: 54px;\n\
   \tmargin-left: 5px;\n\
   \tmix-blend-mode: difference;\n\
   }\n\n\
   #progress {\n\
   \tbackground-color: var(--fg-color);\n\
   \t/* width: 50%; */\n\
   \theight: 100%;\n\
   \t/* transition: width 1s linear; */\n\
   }\n\n\
   #gameprogress {\n\
   \t/* width: 300px; */\n\
   \tmargin: 25px;\n\
   \theight: 15px;\n\
   \t/* background-color: blue; */\n\
   \tmargin-top: 0;\n\
   }\n\n\
   #gameprogress > span {\n\
   \tdisplay: inline-block;\n\
   \t/* height: 10px; */\n\
   \tbackground-color: var(--fg-color);\n\
   \t/* border-left: 1px solid var(--bg-color);\n\
   \tborder-right: 1px solid var(--bg-color); */\n\
   }\n\n\
   /* .prog-waiting {\n\
   \theight: 100%;\n\
   } */\n\
   /* .prog-meditate, .prog-contemplation {\n\
   \theight: 50%;\n\
   } */\n\
   .prog-bead {\n\
   \theight: 100%;\n\
   }\n\
   /* .prog-breath {\n\
   } */\n\n\
   .s-done {\n\
   \topacity: 20%;\n\
   }\n\
   /* .s-active {\n\n\
   } */\n\
   .s-waiting {\n\
   \topacity: 50%;\n\
   }\n\n\n\
   /***** Game config *****/\n\
   .config {\n\
   \tmargin-top: 2em;\n\
   }\n\n\
   summary {\n\
   \ttext-decoration: underline;\n\
   \tcursor: pointer;\n\
   }\n\n\
   button {\n\
   \tfont-size: 140%;\n\
   \tmargin: 10px 0;\n\
   \tcolor: var(--bg-color);\n\
   \t/* color: var(--fg-color); */\n\
   }\n\n\
   details > :first-child {\n\
   \tmargin-bottom: 1em;\n\
   }\n\n\
   label {\n\
   \tmargin-bottom: 3px;\n\
   }\n\
   label > :first-child {\n\
   \tdisplay: inline-block;\n\
   \tmin-width: 14em;\n\
   }\n\n\
   input {\n\
   \twidth: 7em;\n\
   \tfont-size: 16px;\n\
   \t/* color: var(--bg-color); */\n\
   \tborder: 2px solid #686868;\n\
   }\n\n\
   input[type=checkbox] {\n\
   \theight: 1em;\n\
   }\n\n\
   label {\n\
   \tdisplay: block;\n\
   }\n\n\
   #magister_box {\n\
   \tborder: 1px dashed var(--fg-color);\n\
   \t/* margin: 1em 0; */\n\
   \tmargin: 1em auto;\n\
   \tpadding: 0.8em;\n\
   \tmax-width: 500px;\n\
   \tbackground-color: var(--bg-highlight);\n\
   \topacity: 40%;\n\
   \ttransition: opacity 0.3s ease-out;\n\
   }\n\n\
   #magister_box.magister_opaque, #magister_box:hover {\n\
   \topacity: 100%;\n\
   }\n\n\
   #magister_box > button {\n\
   \tdisplay: block;\n\
   \tfont-size: 100%;\n\
   \twidth: 100%;\n\
   \tmargin-top: 0;\n\
   \tpadding: 3px 0;\n\
   }\n\n\
   </style>"
