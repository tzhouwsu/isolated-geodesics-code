# isolated-geodesics-code
post-processing code for geodesic analysis after ChemNetworks

## geodesic: shortest continuous path
Geodesics calculation is implemented in ChemNetworks (https://github.com/AClarkLab/ChemNetworks). It gives the shortest continuous path between two nodes within a graph. It describes the extended connectivity pattern in the network.

## isolated geodesics
Because the ChemNetworks outputs the geodesics between all the possible two nodes, the sub-path that is included in a longer path will also be outputed. For example, path 1 --> 2 --> 3 --> 4 --> 5, will include sub-paths of 1 --> 2 --> 3, 2 --> 3 --> 4 --> 5 ...
So if we are interested in those isolated geodesic paths, i.e. paths are not sub-paths, then post-processing should be used to remove these sub-paths.

This code is aimed at removing the short sub-paths and get all the isolated geodesic paths.

