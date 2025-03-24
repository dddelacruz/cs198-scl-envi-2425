library(INLA)

n = 20
loc = matrix(runif(n*2), n, 2)
mesh = inla.mesh.create(loc, refine=list(max.edge=0.05))
proj = inla.mesh.projector(mesh)
field = cos(mesh$loc[,1]*2*pi*3)*sin(mesh$loc[,2]*2*pi*7)
image(proj$x, proj$y, inla.mesh.project(proj, field))

if (require(rgl)) {
  plot(mesh, rgl=TRUE, col=field, draw.edges=FALSE, draw.vertices=FALSE)
}