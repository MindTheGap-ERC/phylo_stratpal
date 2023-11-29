#install.packages("ape")
#install.packages("phangorn")
#install.packages("phytools")

packages = c("ape", "phangorn", "phytools")
sapply(packages, function(x) invisible(library(x, character.only = TRUE, quietly = TRUE)))
vignette("ape")
vignette("phangorn")
vignette("phytools")
browseVignettes()


set.seed(1)

tr = ape::rlineage(birth = 1.5, death = 0, Tmax = 2)
plot(tr)
axis(1)
axis(2)


library(admtools)
adm = tp_2_adm(t = CarboCATLite_data$time_myr, CarboCATLite_data$height_2_km_offshore_m, T_unit = "Myr", L_unit = "m")
plot(adm)

times = get_all_node_vals(tr)

height = get_height(adm, times, destructive = FALSE)

tr2 = update_branch_lengths(tr,height)

plot(tr2)

plot(tr)
axis(1)
mtext("Time [Myr]", side = 1, line = 2.5)

plot(tr2)
axis(1)
mtext("Stratigraphic Height [m]", side = 1, line = 2.5)

tr$edge
tr$edge.length
tr$tip.label
tr$Nnode
tr$edge.length[6] = 2
plot(tr)
axis(1)
aa = chronos(tr, lambda = 0, calibration = data.frame("node" = find_root(tr,1), "age.min" = 2, "age.max" = 2))

plot(aa)
axis(1)

find_precursor_node=function(x, node){
  
  #'
  #' @title find precursor noode of node in phylo object
  #' 
  #' @param x a phylo object
  #' @param node the node for which the precursor is supposed to be determined
  #' 
  #' @returns integer, id of precursor node
  #' 

  precursor_node=x$edge[,1][x$edge[,2]==node]
  return(precursor_node)
}

#### find root ####
find_root_node = function(x, start_node_id=1){
  
  #'
  #' @title find index of root node in phylo object
  #' 
  #' @param x phylo object
  #' @param start_node integer, node where to start search
  #' 
  #' @returns integer, index of root node
  #' 
  current_node = start_node_id
  repeat{
    precursor_node=find_precursor(x, current_node)
    if(length(precursor_node)==0) break
    current_node=precursor_node
  }
  return(current_node)
}


get_node_vals=function(x, node){
  
  #'
  #' @title get absolute times/pos of nodes
  #' 
  #' @param x phylo object
  #' @param node integer, index of node
  #' 
  #' @returns numeric, absolute location in time/height of node
  #' 
  
  root=find_root(x)
  
  if(node==root){
    if ("root.time" %in% names(x)){
        return(x$root.time)
      } else {
        return(0)
      }
  } else {
    precursor_node=find_precursor(x,node)
    edge_length=x$edge.length[x$edge[,2]==node]
    node_val=get_node_vals(x,precursor_node)+ edge_length
    return(node_val)
  }
}

#### get all node ages ####

get_all_node_vals=function(x){
  
  #' 
  #' @title get time/strat vals of all nodes
  #' 
  #' @param x a phylo object
  #' 
  #' @returns vector with one entry per node, timing/strat for nodes
  #' 
  
  noofnodes=length(x$tip.label)+x$Nnode
  vals=rep(0,noofnodes)
  
  for (i in seq_len(noofnodes)){
    vals[i]=get_node_vals(x,i)
  }
  return(vals)
}

#### update branch lengths ####
update_branch_lengths=function(x,node_vals, root_val = 0){
  
  #'
  #' @title updates branch lengths
  #' 
  #' @param tree a phylo object
  #' @param ages 
  #' 
  #' @returns a phylo object
  for (i in 1:length(x$edge.length)){
    start=x$edge[i,1]
    end=x$edge[i,2]
    x$edge.length[i]=node_vals[end]-node_vals[start]
  }
  x[["root.time"]] = root_val
  return(x)
}

