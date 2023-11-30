devtools::install_github(repo = "MindTheGap-ERC/admtools",
                         ref = "treetransform")
adms = ageDepthModels[["A"]]

names(adms)

adm_list = list()

for (i in 1:(length(adms)-1)){
  adm_list[[i]] = tp_2_adm(t = adms[[i]]$time,
                           h = adms[[i]]$height,
                           T_unit = "Myr",
                           L_unit = "m")
}



tree_time = ape::rlineage(birth = 1.5, death = 0, Tmax = 2)

plot(tr)
axis(1)
mtext("Time [Myr]", side = 1, line = 3)

i = 70

plot(adm_list[[i]])

tree_strat = time_to_strat(tree_time, adm_list[[1]])

plot(tree_strat, direction = "upwards")
axis(2)
mtext("Stratigraphic Height [m]", side = 2, line = 3)
