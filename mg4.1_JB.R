#mg4.1
#works for 100% viable, 
#Clara yip
#3/21/17

#Variables chosen:
#ellipse CI = 0.8
#step direction = uniform 
#distance per step = exp dist(0:maxDist, rate = 6.672)
#crossing
#   stay if step length > 0.25*maxDist
#   else 0.5 probability to cross 


simulateMovement <- function(xLength, yLength, seeds, viable, steps, maxDist, iter) {
  library(plyr)
  library(RColorBrewer)
  library(car)
  library(fields)
  
##### SeedMap: Generates coordinates for forested seeds
  ## For one to the number of seeds, randomly sample one number between 1 and xLength and one number between 1 and ylength. Check to make sure that number hasn't been chosen before. Create a grid that is x by y and give each cell in that grid a value of 0. 
  
  seedMap <- function() {
    for (i in 1:seeds) {
      
      x <- sample(1:xLength, 1)      #chooses random x coor 
      y <- sample(1:yLength, 1)      #chooses random y coor 
      while (!is.na(grid[x, y])) {     #ensures patch hasn't been picked yet
        x <- sample(1:xLength, 1)  
        y <- sample(1:yLength, 1)
      }
      grid[x, y] <<- 0         #sets patch value to 0    
      patchesGrid[x, y] <<- i        #numbers seeds (whatever number seed it is) --> seeding same position on grid and patches grid
      updatePatchesGrid(x, y)       #in case seeds are generated next to each other
      xViable <<- c(xViable, x)       #adds newly viable patch coordinates to lists
      yViable <<- c(yViable, y) #xviable = vector of coordinates of forested patches
    }
  }


##### isInBounds: Checks to see if the x and y coordinates are actually on the grid
  ## takes the x and y coordinate and ensures they are greater than zero and makes sure x is less than or equal to xlength and y is less than or equal to ylength
  isInBounds <- function(x, y) {
    checkX <- (x > 0) && (x <= xLength)
    checkY <- (y > 0) && (y <= yLength)
    return (checkX && checkY) 
  }

  
##### getCoor: Gets the new coordinate after moving x distance in y direction from the original point
  ## Directions are a quadrant and move clockwise beginning with 9:00 == 1, 12:00 == 2, 3:00 == 3, and 4:00 == 4 
  getCoor <- function(x, y, direction, distance) {
    if (direction == 1) {
      newCoor <- c(x-distance, y)
    } else if (direction == 2) {
      newCoor <- c(x, y+distance)
    } else if (direction == 3) {
      newCoor <- c(x+distance, y)
    } else {
      newCoor <- c(x, y-distance)
    }
    return(newCoor)  
  }

  
##### UpdatePatchesGrid: checks to see if any neighboring viable (forested) patches are numbered differently and if so, changes to ensure all are numbered the same
  ## defines the cell one unit to the left of the current cell as "left", one cell above the current cell as "up", by using the getCoor (get new coordinate) function. If XXX and XXX are not equal to zero and XXX is not equal to the current x and y input values (the current cell), the replace that cell (left, right, up, or down) with the curNum (current x number)
  updatePatchesGrid <- function(curX, curY) {
    curNum <- patchesGrid[curX, curY]
    left <- getCoor(curX, curY, 1, 1)
    up <- getCoor(curX, curY, 2, 1)
    right <- getCoor(curX, curY, 3, 1)
    down <- getCoor(curX, curY, 4, 1)
    
    #short-circuits if out of bounds or not-viable 
    if (isInBounds(left[1], left[2]) && patchesGrid[left[1], left[2]] != 0 && patchesGrid[left[1], left[2]] != curNum) {
      #replace all instances of leftNum in patchesGrid with curNum
      patchesGrid[patchesGrid==patchesGrid[left[1], left[2]]] <<- curNum
    } else if (isInBounds(up[1], up[2]) &&  patchesGrid[up[1], up[2]] != 0 && patchesGrid[up[1], up[2]] != curNum) {
      patchesGrid[patchesGrid== patchesGrid[up[1], up[2]]] <<- curNum
    } else if (isInBounds(right[1], right[2]) && patchesGrid[right[1], right[2]] != 0 && patchesGrid[right[1], right[2]] != curNum) {
      patchesGrid[patchesGrid==patchesGrid[right[1], right[2]]] <<- curNum
    } else if (isInBounds(down[1], down[2]) && patchesGrid[down[1], down[2]] != 0 && patchesGrid[down[1], down[2]] != curNum) {
      patchesGrid[patchesGrid==patchesGrid[down[1], down[2]]] <<- curNum
    }
    
  }
  

##### nucleateMap: Calculates how many cells need to be added to the seed to create the desired percent forest cover
  ## viable = percent forest cover from 10:100 in increments of 10. viablePatches calculates the number of cells needed to be "viable" in order to reach the desired percent of forest cover. To create the forested patch, from one to the number of viable patches minus the number of seeds, choose a coordinate from the xviable/yviable list. Then choose a random direction. Get the x and y coordinate of the new point. Test to see if that point is in bounds and is not an existing point. Give it a value of zero and then update the x and y coordiantes to "added = true", meaning they've been added as viable/forested cells

  
  nucleateMap <- function() {
    viablePatches <- as.integer(viable/100 * area)
    for (i in 1:(viablePatches-seeds)) {
      added <- FALSE
      while (!added) {
        index <- sample(1:length(xViable), 1)
        x <- xViable[index] #just the value of how far into the list we want to go so it does choose a pair
        y <- yViable[index]
        direction <- sample(1:4, 1)
        toAdd <- getCoor(x, y, direction, 1)
        nextX <- toAdd[1]
        nextY <- toAdd[2]
        if (isInBounds(nextX, nextY) && is.na(grid[nextX, nextY])) {
          grid[nextX, nextY] <<- 0  #mark it as viable by making it a zero 
          patchesGrid[nextX, nextY] <<- patchesGrid[x,y] # and then number it based on which seed it nucleated from  
          updatePatchesGrid(nextX, nextY)
          xViable <<- c(xViable, nextX)
          yViable <<- c(yViable, nextY)
          added <- TRUE 
        }
      }
    }
  }

  
##### addSecondaries: creating a list of the viable/forested cell coordinates
  ## from 1 to dist, get the random new coordinate x and y values. Create a table with a row of x values and a row of y values
  
  #adds patches along the next path 
  addSecondaries <- function(x1, y1, dir, dist) {
    xPath <- c()
    yPath <- c()
    x <- x1
    y <- y1
    for (i in 1:dist) {
      nextCoor <- getCoor(x, y, dir, 1)
      x <- nextCoor[1]
      y <- nextCoor[2]
      xPath <- c(xPath, x)
      yPath <- c(yPath, y)
    }
    return(rbind(xPath, yPath))
  }

  
##### allViable: tests to see if every patch in the path is viable/forested
  ## first row of values from 'path' are x values and second row are y values. Check to make sure the x value is ...*** Check to see if the xviable path can be done without accessing the 'crossing' function
  #end point is in bounds -> all points are in bounds
  allViable <- function(path) {
    xPath <- path[1,]
    yPath <- path[2,]
    for (i in 1:length(xPath)) {
      if (is.na(grid[xPath[i], yPath[i]])) {
        return(FALSE)
      }
    }
    return(TRUE)
  }


##### countNonViable: counts number of cells in the predicted path that are non-viable 
  ## initialize the counter to a value of zero. For each coordinate in the path, counter = counter + 1 if that coordinate lands on a cell with a value of 'na'
  
  #counts number of patches/squares in path that are non-viable
  countNonViable <- function(path) {
    xCoors <- path[1,]
    yCoors <- path[2,]
    len <- length(xCoors)
    counter <- 0
    for (i in 1:len) {
      if (is.na(grid[xCoors[i], yCoors[i]])) {
        counter <- counter + 1
      }
    }
    return(counter)
  }

  
##### chooseCross: peccary decides whether or not to cross the matrix (non-viable land).
  ## If the number of non-viable cells is greater than 0.25 * max step distance, then the peccary stays on the cell it's at. Else, make a decision whether to cross (choose 1 or 2 so that's a 50/50 chance of crossing). If it's a 1, then the peccary stays where it is the "hit" count of that cell increases by 1. If a 2 is chosen, then the peccary crosses the matrix [to another forested patch]
  ## Clara, we don't increase the counter if they stay on that same patch, correct? only if they cross?
  
  #end point is viable, not all patches in path are viable
  chooseCross <- function(path, dist) {
    if (countNonViable(path) > 0.25*maxDist) { ################# CHANGE VALUE
      return(rbind(path[1], path[2]))  #stay
    } else {
      decision <- sample(1:2, 1)    ##############50/50 prob
      if (decision == 1) {  #stay
        return(rbind(path[1], path[2]))
      } else { #cross
        crossed <<- crossed + 1
        return(path)
      }
    }
  }

  

##### nextPath: generates a vector of the coordinates of the next path that the peccary is scheduled to take
  ## Randomly get a direction (left, right, up, or down). Randomly choose a step length from the exponential distribution, multiply it by the max dist and add 1. Based on this direction and this step length, get the coordinates of the endpoint for this path. If this endpoint is in bounds and is on a forested patch then, return the coordinates of the path if the path is all forested. If the path is not all forested, go to the choosecross function to determine whether to cross or stay.
  
  #generates a pair of vectors 
  #returns coordinates of next path
  nextPath <- function(x, y) {
    #choose end point (dir + dist)
    genDir <- sample(1:4, 1)
    genDist <- as.integer(rexp(1,rate=6.672) * maxDist) + 1   
    endPoint <- getCoor(x, y, genDir, genDist)
    
    if (isInBounds(endPoint[1], endPoint[2]) && !is.na(grid[endPoint[1], endPoint[2]])) {   #check if na, can't end in non-forested land b/c na= matrix
      path <- addSecondaries(x, y, genDir, genDist)
      if (allViable(path)) { ##allviable means we can do this path without crossing
        return(path)
      } else {  #if not, we do have to cross so we go to the choose cross whole probability function thing
        path <- chooseCross(path, genDist)
        return(path)
      }
      return(path)
    }
    return(c())
  }

  
  
##### walkPath: peccary moves across the chosen path and the counter on the grid matrix records the number of times a peccary hits each cell
  ## divide the path vector into x coordinates and y coordinates. For each coordinate, if the x and y coordinates are not na on the grid matrix, count = count + 1. If they are na? 
  
  walkPath <- function(path) {
    xCoors <- path[1,]
    yCoors <- path[2,]
    len <- length(xCoors)
    
    for (i in 1:len) {
      if ((!is.na(grid[xCoors[i], yCoors[i]]))) {
        grid[xCoors[i], yCoors[i]] <<- grid[xCoors[i], yCoors[i]] + 1  #grid holds the number of times it's been treaded over. So this is our counter. counter = counter + 1 with each tread
      }
    }
    last <- c(xCoors[len], yCoors[len])
    return(last)
  }

 
   
##### simulateMovement: ***This is a simulatemovement function within the larger simulatemovement function? What is this part specifically doing?
  ## Choose a starting coordinate from the viable array. For each step, ...?
  
  simulateMovement <- function() {
    startIndex <- sample(1:length(xViable), 1)
    startX <- xViable[startIndex]
    startY <- yViable[startIndex]
    
    for (i in 1:steps) {
      path <- nextPath(startX, startY)
      while (is.null(path)) {
        path <- nextPath(startX, startY)
      }
      endIndex <- walkPath(path)
      startX <- endIndex[1]
      startY <- endIndex[2]
    }
  }
  
  
  
  
##### avgDistPatches: Calculates the distance between forested patches and records only the n shortest distances, where n = #unique patches minus one
  ## get all unique numbers on the patchesGrid matrix and sort in ascending order (numbers the forested patches uniquely). The number of patches is 1 minus the length of that list. Renumber each patch starting with 0. Create a matrix ...***
  
  avgDistPatches <- function() {
    #number of patches
    uniq <- sort(unique(as.vector(patchesGrid)), decreasing = FALSE)
    patchNum <- length(uniq) - 1
    
    
    if(patchNum == 0) {
      return(0)
    }
    
    #renumber patches in patchesGrid
    ord <- 0:patchNum
    for (i in 1:length(uniq)) {
      patchesGrid[patchesGrid==uniq[i]] <<- ord[i]
    }
    
    patchVecs <- matrix(rep(list(), patchNum*2), nrow = patchNum, ncol = 2) #patch coor holder
    #for every forested patch, add to corresponding list 
    for (i in 1:length(xViable)) {
      num <- patchesGrid[xViable[i], yViable[i]]
      patchVecs[[num]] <- c(patchVecs[[num]], xViable[i])
      patchVecs[[num+patchNum]] <- c(patchVecs[[num+patchNum]], yViable[i])
    }
    
    #image(1:xLength, 1:yLength, patchesGrid, col =  brewer.pal(5, "OrRd"))
    plot(xViable, yViable)
    
    patchConf <- matrix(rep(list(), patchNum), nrow = patchNum, ncol = 1)  #ellipse coors holder
    #draw ellipse for each patch
    for (i in 1:patchNum) {     #confidence levels mutable, must change dataframe result as well 
      patchConf[[i]] <-  dataEllipse(patchVecs[[i]], patchVecs[[i+patchNum]], levels=c(0.8), center.pch=19, center.cex=1.5, plot.points=FALSE)
    }
    
    #calc distance min distance between patch ellipses
    if (patchNum == 1) {
      return(0)
    } else {
      distHolder <- vector()
      for (i in 1:patchNum) {
        for (j in 1:patchNum) {
          if (i != j && i < j) {  #no repeat combinations
            minDist <- min(rdist(patchConf[[i]], patchConf[[j]]))  #calculates minimum distance between two patches 
            distHolder <- c(distHolder, minDist)
          }
        }
      }
      #print(distHolder)
      return(mean(distHolder))
    }
  }

  
  
##### Begin model here
  
  
  
  #generate matrices for landscape and movement
  grid <- matrix(NA, nrow = xLength, ncol = yLength) #viable / non-viable and movement
  patchesGrid <- matrix(0L, nrow = xLength, ncol = yLength) #viable / nonviable put just uses it as a counter for the number of patches
  
  #seed map
  xViable <- vector()
  yViable <- vector()
  seedMap()
  
  #nucleate map
  area <- xLength * yLength
  if (viable > 100 || viable < (seeds/area)) { ## if viable is greater than 100 or it's less than seeds/area, note that this is invalid
    print("percent viable invalid")
    break
  }
  nucleateMap()
  
  
  #save & output results
  #create file name
  fileName <- paste("Uniform", toString(seeds), toString(viable), toString(steps), iter, sep="_")
  
  
  #create/save the 'before' map visual 
  grid1 <- replace(grid, is.na(grid), -10)
  img <- image(1:xLength, 1:yLength, grid1, col =  brewer.pal(3, "OrRd")) 
  fileNameJPGMap <- paste(fileName, "grid", "before", ".jpeg", sep = "")
  dev.copy(jpeg, fileNameJPGMap)
  dev.off()
  
  crossed <- 0
  simulateMovement() 
  avgDist <- avgDistPatches()
  
  #create/save the 'post' map visual 
  grid1 <- replace(grid, is.na(grid), -10)
  img <- image(1:xLength, 1:yLength, grid1, col =  brewer.pal(9, "OrRd")) 
  fileNameJPGMap <- paste(fileName, "grid", "post", ".jpeg", sep = "")
  dev.copy(jpeg, fileNameJPGMap)
  dev.off()
  
  #create/save histogram recording the frequency distribution of hit cells
  hist(grid, freq=TRUE, xlab = "Number of times treaded", col =  brewer.pal(10, "Spectral"), breaks=20,  xlim = c(0,120), ylim= c(0,4000))
  fileNameJPGHist <- paste(fileName, "hist", ".jpeg", sep = "")
  dev.copy(jpeg, fileNameJPGHist)
  dev.off()
  
  #create/save frequency table of number of cell hits and their frequency of occurence --> *** same data as histogram uses, correct?
  maxTread <- max(grid, na.rm =TRUE)
  nums <- c(0:maxTread)
  freqAll <- count(as.vector(grid))
  freq0 <- freqAll[1,2]
  frequencies <- c(freq0, tabulate(as.vector(grid)))
  freq <- setNames(frequencies, nums)
  viablePatches <- as.integer(viable/100 * area)
  percents <- frequencies/as.integer(viablePatches)
  freq = rbind(freq, percents)
  fileNameCSV <- paste(fileName, "freq", ".csv", sep = "")
  write.csv(freq, file = fileNameCSV)
  
  return(c(freq[2,1], crossed, avgDist)) 
}

