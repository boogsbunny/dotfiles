// ============================================================================
// user.js
//
// Goals, in order:
//   1. Use system DPI correctly via Xft.dpi, not Firefox-only scaling hacks.
//   2. Offload video decode to the iGPU where supported.
//   3. Keep the NVMe idle — cache in RAM, write session state rarely.
//   4. Cut background network chatter.
// Without breaking sites or weakening security (Safe Browsing + updates stay on).
//
// Applied at every Firefox startup; restart Firefox to load.
// Confirm active profile in about:support -> Profile Directory.
// ============================================================================

// ---------------------------------------------------------------------------
// 1. Display + GPU policy
// ---------------------------------------------------------------------------

// Use system DPI. With ~/.Xresources containing `Xft.dpi: 192`, this gives
// Firefox 2x scaling without forcing Firefox-specific DPR.
user_pref("layout.css.devPixelsPerPx", "-1.0");

// Keep VA-API video decode enabled.
// Needs LIBVA_DRIVER_NAME=iHD + intel-media-driver on obelisk.
user_pref("media.ffmpeg.vaapi.enabled", true);
user_pref("media.hardware-video-decoding.enabled", true);

// Do NOT force these unless testing proves they help on this machine.
// Let Firefox choose the safest compositor/backend for X11 + Mesa.
//
// user_pref("gfx.webrender.all", true);
// user_pref("gfx.x11-egl.force-enabled", true);
// user_pref("gfx.canvas.accelerated", true);

// ---------------------------------------------------------------------------
// 2. Keep the NVMe idle — cache in RAM, persist state rarely.
// ---------------------------------------------------------------------------
user_pref("browser.cache.disk.enable", false);              // disk cache -> RAM
user_pref("browser.cache.memory.enable", true);
user_pref("browser.cache.memory.capacity", 1048576);        // 1 GiB, value is KiB
user_pref("media.memory_cache_max_size", 65536);            // 64 MiB media cache
user_pref("browser.sessionstore.interval", 300000);         // crash snapshot every 5 min
user_pref("browser.sessionstore.max_tabs_undo", 10);
user_pref("browser.sessionhistory.max_total_viewers", -1);  // auto-size bfcache by RAM

// ---------------------------------------------------------------------------
// 3. Cut background network + periodic writes.
//    Security-relevant polling is intentionally LEFT ON.
// ---------------------------------------------------------------------------
user_pref("toolkit.telemetry.enabled", false);
user_pref("toolkit.telemetry.unified", false);
user_pref("toolkit.telemetry.archive.enabled", false);
user_pref("datareporting.healthreport.uploadEnabled", false);
user_pref("datareporting.policy.dataSubmissionEnabled", false);
user_pref("app.shield.optoutstudies.enabled", false);
user_pref("app.normandy.enabled", false);
user_pref("browser.discovery.enabled", false);
user_pref("browser.ping-centre.telemetry", false);
user_pref("browser.newtabpage.activity-stream.feeds.telemetry", false);
user_pref("browser.newtabpage.activity-stream.telemetry", false);
user_pref("network.connectivity-service.enabled", false);

// ---------------------------------------------------------------------------
// 4. Responsiveness
// ---------------------------------------------------------------------------
user_pref("browser.tabs.unloadOnLowMemory", true);

// ============================================================================
// Deliberately NOT set:
//
//   - gfx.webrender.all
//   - gfx.x11-egl.force-enabled
//   - gfx.canvas.accelerated
//
//     These can be useful for experiments, but should not be forced permanently
//     on this X11/EGL/UHD 620 setup unless measured.
//
//   - network.http.pipelining / max-connections bumps
//     Pipelining is removed; connection bumps rarely help.
//
//   - nglayout.initialpaint.delay / content.notify.*
//     Pre-Quantum reflow knobs; no useful effect on modern Firefox.
//
//   - dom.ipc.processCount overrides
//     Fission allocates processes dynamically.
//
//   - browser.safebrowsing.* = false
//     Security downgrade, not a real perf win.
//
//   - accessibility.force_disabled = 1
//     Can break screen readers and some extensions; a11y is lazy-loaded anyway.
// ============================================================================

user_pref("browser.urlbar.scotchBonnet.enableOverride", false);
