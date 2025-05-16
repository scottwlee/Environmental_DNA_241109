# ============================================================================
# Function: zi.pi
# Purpose: Calculate within-module (Zi) and among-module (Pi) connectivity
# Inputs:
#   - nodes_bulk: dataframe with node metadata including degree and modularity class
#   - z.bulk: binary adjacency matrix (unweighted)
#   - modularity_class: column name in nodes_bulk indicating module membership
#   - degree: column name in nodes_bulk indicating node degree
# Output:
#   - Dataframe with Zi, Pi, and merged node metadata
# ============================================================================

zi.pi<-function(nodes_bulk, z.bulk, modularity_class, degree){

## SECTION 1: Initialization -----------------------------------------------
z.bulk[abs(z.bulk)>0]<-1 # convert to binary adjacency matrix
module<-which(colnames(nodes_bulk)==modularity_class)
module.max<-max(nodes_bulk[,module])
degree<-which(colnames(nodes_bulk)==degree)

## SECTION 2: Split adjacency matrix by module ----------------------------
bulk.module<-list(NA)
length(bulk.module)<-module.max

for(i in 1:max(nodes_bulk[,module])){
	bulk.module[[i]]<-z.bulk[which(nodes_bulk[,module]==i),which(nodes_bulk[,module]==i)]
	bulk.module[[i]]<-as.data.frame(bulk.module[[i]])
	rownames(bulk.module[[i]])<-rownames(z.bulk)[which(nodes_bulk[,module]==i)]
	colnames(bulk.module[[i]])<-colnames(z.bulk)[which(nodes_bulk[,module]==i)]
}

## SECTION 3: Calculate Zi (within-module connectivity) -------------------
z_bulk<-list(NA)
length(z_bulk)<-module.max

for(i in 1:length(z_bulk)){
	z_bulk[[i]]<-bulk.module[[i]][,1]
	z_bulk[[i]]<-as.data.frame(z_bulk[[i]])
	colnames(z_bulk[[i]])<-"z"
	rownames(z_bulk[[i]])<-rownames(bulk.module[[i]])
}

for(i in 1:max(nodes_bulk[,module])){
	if(length(bulk.module[[i]])==1){
		z_bulk[[i]][,1]<-0
	}else if(sum(bulk.module[[i]])==0){
		z_bulk[[i]][,1]<-0
	}else{
		k<-rowSums(bulk.module[[i]]) -1
		mean<-mean(k)
		sd<-sd(k)
		if (sd==0){
		    z_bulk[[i]][,1]<-0
		}else{
		    z_bulk[[i]][,1]<-(k-mean)/sd
		}
	}
}

# Merge Zi values into a single dataframe
for(i in 2:max(nodes_bulk[,module])) {
	z_bulk[[i]]<-rbind(z_bulk[[i-1]],z_bulk[[i]])
}
z_bulk<-z_bulk[[module.max]]

## SECTION 4: Split adjacency matrix columns by module (for Pi) ----------
bulk.module1<-list(NA)
length(bulk.module1)<-module.max

for(i in 1:max(nodes_bulk[,module])){
	bulk.module1[[i]]<-z.bulk[,which(nodes_bulk[,module]==i)]
	bulk.module1[[i]]<-as.data.frame(bulk.module1[[i]])
	rownames(bulk.module1[[i]])<-rownames(z.bulk)
	colnames(bulk.module1[[i]])<-colnames(z.bulk)[which(nodes_bulk[,module]==i)]
}

## SECTION 5: Initialize Pi containers ------------------------------------
c_bulk<-list(NA)
length(c_bulk)<-module.max

for(i in 1:length(c_bulk)){
	c_bulk[[i]]<-z.bulk[,1]
	c_bulk[[i]]<-as.matrix(c_bulk[[i]])
	colnames(c_bulk[[i]])<-"c"
	rownames(c_bulk[[i]])<-rownames(z.bulk)
	c_bulk[[i]][,1]<-NA
}

## SECTION 6: Calculate squared connections to each module ---------------
for(i in 1:max(nodes_bulk[,module])){
	c_bulk[[i]]<-rowSums(bulk.module1[[i]])
	c_bulk[[i]]<-as.matrix(c_bulk[[i]])
	c_bulk[[i]][which(nodes_bulk$modularity == i),] = c_bulk[[i]][which(nodes_bulk$modularity == i),] -1
	c_bulk[[i]]<-c_bulk[[i]]*c_bulk[[i]]
	colnames(c_bulk[[i]])<-"c"
	rownames(c_bulk[[i]])<-rownames(z.bulk)
}

## SECTION 7: Sum over all modules and calculate Pi -----------------------
for(i in 2:max(nodes_bulk[,module])){
	c_bulk[[i]]<-c_bulk[[i]]+c_bulk[[i-1]]
}
c_bulk<-c_bulk[[module.max]]

# Final Pi calculation per node
for(i in 1: length(c_bulk)){
if(nodes_bulk$degree[i]==1){
        c_bulk[i] <- 0
        }else{
            c_bulk[i] <- 1-(c_bulk[i]/(nodes_bulk$degree[i]*nodes_bulk$degree[i]))
        }
}
colnames(c_bulk)<-"c"

## SECTION 8: Merge Zi and Pi with metadata -------------------------------
z_c_bulk<-c_bulk
z_c_bulk<-as.data.frame(z_c_bulk)
z_c_bulk$z<-z_bulk[match(rownames(c_bulk),rownames(z_bulk)),]
z_c_bulk<-z_c_bulk[,c(2,1)]
names(z_c_bulk)[1:2]<-c('within_module_connectivities','among_module_connectivities')

# Add node IDs and merge with original metadata
z_c_bulk$nodes_id<-rownames(z_c_bulk)
nodes_bulk$nodes_id<-rownames(nodes_bulk)
z_c_bulk<-merge(z_c_bulk,nodes_bulk,by='nodes_id')
z_c_bulk
}
