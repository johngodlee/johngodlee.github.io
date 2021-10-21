//As well as the object areas, this macro also returns a list of how many objects were measured in each file
run("8-bit");
setAutoThreshold("Default");
//run("Threshold...");
//setThreshold(0, 146);
setOption("BlackBackground", false);
run("Convert to Mask");
run("Set Scale...", "distance=120.006 known=1 pixel=1 unit=cm global");
run("Analyze Particles...", "size=0.4-Infinity circularity=0.1-1.00  label=null show=Outlines display add");
setOption("Display Label", true);
nROIs = roiManager("count");
print(nROIs);




