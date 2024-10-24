
#you need these libraries-----

library(ggplot2)
library(patchwork)


# The function-----------

seqtree_plot <- function(seqt, seqdat){
  
  # seqt <- fam.ward.tree
  # 
  # seqdat <- activity.year.seq
  
  tree_plots <- list()
  
  #root plot
  
  root <- seqt$root$info$ind
  
  ind <- seqt$root$info$ind
  
  n_split <- seqt$root$info$splitschedule
  
  node_id <- 0
  
  split_var <- attributes(seqt$terms)$term.labels[seqt$root$split$varindex]
  
  split_vals <- seqt$root$split$labels
  
  split_pops <- c(right=seqt$root$split$info$rpop, left=seqt$root$split$info$lpop)
  
  depth <- seqt$root$info$depth
  
  
  plot <- ggseqplot::ggseqdplot(seqdat[ind,])+labs(title=paste("n=", length(ind)), 
                                                   caption=paste("split no.", n_split, split_var))+
    theme(legend.position = "none")+theme(plot.margin = margin(5, 5, 5, 5, "pt"), 
                                          axis.title.y = element_blank(),
                                          plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5, size=14))
  
  pos <- c(0,0)
  
  right_left <- "root"
  
  tree_plots[[n_split]] <- list(n_split=n_split, split_var=split_var, plot=plot, pos=pos, split_vals=split_vals, split_pops=split_pops, 
                                node_id=node_id, depth=depth, right_left=right_left)
  
  kids_list <- list()
  
  kids_list <- seqt$root$kids
  
  #kids plot
  
  
  
  
  
  kids_list <- lapply(kids_list, function(x){
    
    c(x, parent=0)
  })
  
  
  
  #kid 1
  
  x <- 1
  
  kids_vec <- unlist(seqt$root)[grepl('kids',names(unlist(seqt$root)),fixed=T)]
  
  kids_length <- length(kids_vec[grep("\\.id", names(kids_vec))])
  
  while (x<(kids_length+1)) {
    
    #ind <- kids[[x]]$info$ind
    
    
    ind <- kids_list[[x]]$info$ind
    
    parent <- kids_list[[x]]$parent
    
    
    n_split <- kids_list[[x]]$info$splitschedule
    
    node_id <- paste(n_split, x, sep="_")
    
    split_pops <- c(right=kids_list[[x]]$split$info$rpop, left=kids_list[[x]]$split$info$lpop)
    
    
    
    
    parent_ind <- which(
      unlist(lapply(tree_plots, function(x){
        
        x$node_id
      }))==parent)
    
    
    tree_plots[[parent_ind]]$split_var
    
    
    right_left <- ifelse(kids_list[[x]]$info$n==tree_plots[[parent_ind]]$split_pops[2],"left", "right")
    
    split_var <- attributes(seqt$terms)$term.labels[kids_list[[x]]$split$varindex]
    
    title_lab <- ifelse(right_left=="left", paste(tree_plots[[parent_ind]]$split_var, tree_plots[[parent_ind]]$split_vals[1]),
                        paste(tree_plots[[parent_ind]]$split_var,tree_plots[[parent_ind]]$split_vals[2]))
    
    split_val <- ifelse(right_left=="left", tree_plots[[parent_ind]]$split_vals[1],tree_plots[[parent_ind]]$split_vals[2])
    
    depth <- kids_list[[x]]$info$depth
    
    
    
    plot <- ggseqplot::ggseqdplot(seqdat[ind,])+labs(title=paste(split_val,"n=", length(ind)), 
                                                     caption=paste("split no.", n_split, split_var))+
      theme(legend.position = "none")+theme(plot.margin = margin(5, 5, 5, 5, "pt"), 
                                            axis.title.y = element_blank(),
                                            plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5, size=14))
    
    #prevent overlap in x-positions
    
    # tree_plots[[parent_ind]]$right_left
    # 
    # tree_plots[[1]]$right_left
    
    # if (tree_plots[[parent_ind]]$right_left=="left"){
    #   
    #   #pos_x <- pos_x-1
    #   
    #   tree_plots[[parent_ind]]$pos[1] <- tree_plots[[parent_ind]]$pos[1]-1
    #   
    # } else if (tree_plots[[parent_ind]]$right_left=="right") {
    #   
    #   tree_plots[[parent_ind]]$pos[1] <- tree_plots[[parent_ind]]$pos[1]+1
    #   
    # }
    
    
    if (tree_plots[[parent_ind]]$right_left=="root"){
      
      
      pos_x <- ifelse(right_left=="left", tree_plots[[parent_ind]]$pos[1]-2, 
                      tree_plots[[parent_ind]]$pos[1]+2)
      
    } else {
      
      pos_x <- ifelse(right_left=="left", tree_plots[[parent_ind]]$pos[1]-1, 
                      tree_plots[[parent_ind]]$pos[1]+1)
    }
    
    
    
    
    
    #prevent overlap in x-positions
    
    
    # if (tree_plots[[parent_ind]]$right_left=="left"){
    #   
    #   pos_x <- pos_x-1
    #   
    # } else if (tree_plots[[parent_ind]]$right_left=="right") {
    #   
    #   pos_x <- pos_x+1
    #   
    # }
    
    
    pos <- c(pos_x,tree_plots[[parent_ind]]$pos[2]+.5)
    
    list_ind <- length(tree_plots)+1
    
    
    
    
    tree_plots[[list_ind]] <-list(n_split=n_split, split_var=split_var, plot=plot, pos=pos, split_vals=split_vals, split_pops=split_pops, 
                                  node_id=node_id, depth=depth, right_left=right_left, parent_ind=parent_ind)
    
    kids <- kids_list[[x]]$kids
    
    kids <- lapply(kids, function(x){
      
      c(x, parent=node_id)
    })
    
    kids_list <- c(kids_list, kids)
    
    x <- x+1
    
  }
  
  
  
  
  root_x <- 10
  
  #length(tree_plots)
  
  positions <- rep(0,length(tree_plots))
  
  
  
  for(y in 1:length(tree_plots)){
    
    #y <- 4
    
    x <- tree_plots[[y]]
    
    
    par_ind <- x$parent_ind
    
    
    
    if (x$right_left=="root") {
      
      positions[y] <- root_x
      
      pos_x <- root_x+x$pos[1]
      
    } else {
      
      
      
      #pos_x <- positions[par_ind]+x$pos[1]
      
      pos_x <- ifelse(x$right_left=="left", positions[par_ind]-1, positions[par_ind]+1)
      
      positions[y] <- pos_x
      
    }
    
  }
  
  pos_vec <- lapply(1:length(tree_plots), function(y){
    
    #y <- 2
    
    x <- tree_plots[[y]]
    
    
    par_ind <- x$parent_ind
    
    
    
    if (x$right_left=="root") {
      
      #positions[y] <<- root_x
      
      pos_x <- root_x+x$pos[1]
      
    }  else {
      
      node_pos <- ifelse(x$right_left=="left",-1,1)
      
      pos_x <- positions[par_ind]+node_pos
      
      
    }
    
    #pos_x <- root_x+x$pos[1]
    
    
    
    pos_y <- x$depth
    
    if (x$right_left=="root"){
      
      area <- area(t = pos_y , l = pos_x, b = pos_y, r = pos_x)
      
    } else if (tree_plots[[par_ind]]$right_left=="root") {
      
      if (x$right_left=="left"){
        
        pos_x <- pos_x-1
        
        area <-area(t = pos_y , l = pos_x, b = pos_y, r = pos_x)
        
        area[which(area<1)] <- 1
        
        
        
      } else {
        
        pos_x <- pos_x+2
        
        area <- area(t = pos_y , l = (pos_x), b = pos_y, r = (pos_x))
        
        area[which(area<1)] <- 1
        
      }
      
      
      
    } else if ((tree_plots[[par_ind]]$right_left=="left")) {
      
      #pos_x <- pos_x-1
      
      if (x$right_left=="right"){
        
        pos_x <- pos_x-1
        
        #area <-area(t = pos_y , l = pos_x, b = pos_y, r = pos_x+1)
        
        area <-area(t = pos_y , l = pos_x, b = pos_y, r = pos_x)
        
        
        area[which(area<1)] <- 1
        
        
        
      } else {
        
        #area <- area(t = pos_y , l = (pos_x)-1, b = pos_y, r = (pos_x))
        
        area <- area(t = pos_y , l = (pos_x), b = pos_y, r = (pos_x))
        
        
        area[which(area<1)] <- 1
        
      }
      
      
    } else if ((tree_plots[[par_ind]]$right_left=="right")) {
      
      #pos_x <- pos_x-1
      
      
      if (x$right_left=="right"){
        
        #area <-area(t = pos_y , l = pos_x, b = pos_y, r = pos_x+1)
        
        area <-area(t = pos_y , l = pos_x, b = pos_y, r = pos_x)
        
        
        area[which(area<1)] <- 1
        
        
        
      } else {
        
        pos_x <- pos_x+1
        
        
        #area <- area(t = pos_y , l = (pos_x)-1, b = pos_y, r = (pos_x))
        
        area <-area(t = pos_y , l = pos_x, b = pos_y, r = pos_x)
        
        
        area[which(area<1)] <- 1
        
        
      }
      
    }
    
    positions[y] <<- pos_x
    
    
    
    # area <-area(t = pos_y , l = abs(pos_x), b = pos_y, r = abs(pos_x))
    # 
    # area[which(area<1)] <- 1
    
    #c(pos_x, pos_y)
    
    # if (x$pos[1]<0){
    # 
    #   area <-area(t = pos_y , l = pos_x, b = pos_y, r = pos_x+1)
    # 
    #   area[which(area<1)] <- 1
    # 
    # 
    # 
    # } else if (x$pos[1]<0) {
    # 
    #   area <- area(t = pos_y , l = (pos_x)-1, b = pos_y, r = (pos_x))
    # 
    #   area[which(area<1)] <- 1
    # 
    # 
    # } else {
    # 
    #   area <- area(t = pos_y , l = pos_x-.5, b = pos_y, r = pos_x+.5)
    # 
    #   area[which(area<1)] <- 1
    # }
    # 
    return(area)
    
    
    
  })
  
  
  plots_sub <- lapply(tree_plots,function(x){
    
    x$plot
  })
  
  #plots_sub <- plots_sub[1:13]
  
  
  layout <- do.call(c, pos_vec)
  
  
  
  patchwork::wrap_plots(plots_sub, design=layout)
  
  
}


#example use--------

library(TraMineR)

data(mvad)


## Defining a state sequence object
seqdat <- seqdef(mvad[, 17:86])

## Growing a seqtree from Hamming distances:
##   Warning: The R=10 used here to save computation time is
##   much too small and will generate strongly unstable results.
##   We recommend to set R at least as R=1000.
##   To comply with this small R value, we set pval = 0.1.
seqt <- seqtree(seqdat~ male + Grammar + funemp + gcse5eq + fmpr + livboth,
                data=mvad, R=10, pval=0.1, seqdist.arg=list(method="HAM", norm="auto"))

seqtree_plot(seqt=seqt, seqdat=seqdat)