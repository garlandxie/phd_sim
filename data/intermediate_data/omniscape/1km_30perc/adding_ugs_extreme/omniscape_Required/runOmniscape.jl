cd(raw"C:\Users\Garland\Documents\R\phd_sim\syncrosim\ugs_sim.ssim.data\Scenario-136\omniscape_Required")

using Pkg; Pkg.add(name="GDAL"); Pkg.add(name="Omniscape")
using Omniscape
run_omniscape("config.ini")