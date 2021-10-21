// Automatically calculate the area of dark objects (leaves) against a white background.
// 79.7619px/cm a4-200dpi.  
// 120.006px/cm a4-300dpi.
// Change the size min to analyse smaller leaves, but risk picking up particles or noise.

run("8-bit");
setAutoThreshold("Default");
//run("Threshold...");
//setThreshold(0, 146);
setOption("BlackBackground", false);
run("Convert to Mask");
run("Set Scale...", "distance=120.006 known=1 pixel=1 unit=cm global");
run("Analyze Particles...", "size=0.70-Infinity show=Outlines display add");
setOption(“Display Label”, true)

