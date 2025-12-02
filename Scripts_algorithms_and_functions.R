##################Scripts, algorithms and functions#################

############scripts##############
print("Hello geocompr")


if (!file.exists("required_geo_data.gpkg")) {
  message("No file, required_geo_data.gpkg is missing!")
}
poly_mat = cbind(
  x = c(0, 9, 9, 0, 0),
  y = c(0, 0, 9, 9, 0)
)
# Short URL to code/11-centroid-alg.R in the geocompr repo
source("https://t.ly/0nzj")


########Geometric algorithms#####
# generating a simple matrix representation of a polygon:
x_coords = c(10, 20, 12, 0, 0, 10)
y_coords = c(0, 15, 20, 10, 0, 0)
poly_mat = cbind(x_coords, y_coords)

#now we create a point representing the origin:
Origin = poly_mat[1, ]
# create 'triangle matrix':
T1 = rbind(Origin, poly_mat[2:3, ], Origin) 
C1 = (T1[1,] + T1[2,] + T1[3,]) / 3
plot(C1)

# calculate the area of the triangle represented by matrix T1:
abs(T1[1, 1] * (T1[2, 2] - T1[3, 2]) +
      T1[2, 1] * (T1[3, 2] - T1[1, 2]) +
      T1[3, 1] * (T1[1, 2] - T1[2, 2])) / 2

i = 2:(nrow(poly_mat) - 2)
T_all = lapply(i, function(x) {
  rbind(Origin, poly_mat[x:(x + 1), ], Origin)
})

C_list = lapply(T_all,  function(x) (x[1, ] + x[2, ] + x[3, ]) / 3)
C = do.call(rbind, C_list)

A = vapply(T_all, function(x) {
  abs(x[1, 1] * (x[2, 2] - x[3, 2]) +
        x[2, 1] * (x[3, 2] - x[1, 2]) +
        x[3, 1] * (x[1, 2] - x[2, 2]) ) / 2
}, FUN.VALUE = double(1))


source("code/11-centroid-alg.R")


##########functions####################
t_centroid = function(x) {
  (x[1, ] + x[2, ] + x[3, ]) / 3
}

t_centroid(T1)


##using t_area
t_area = function(x) {
  abs(
    x[1, 1] * (x[2, 2] - x[3, 2]) +
      x[2, 1] * (x[3, 2] - x[1, 2]) +
      x[3, 1] * (x[1, 2] - x[2, 2])
  ) / 2
}

t_area(T1)

t_new = cbind(x = c(0, 3, 3, 0),
              y = c(0, 0, 1, 0))
t_area(t_new)



poly_centroid = function(poly_mat) {
  Origin = poly_mat[1, ] # create a point representing the origin
  i = 2:(nrow(poly_mat) - 2)
  T_all = lapply(i, function(x) {rbind(Origin, poly_mat[x:(x + 1), ], Origin)})
  C_list = lapply(T_all, t_centroid)
  C = do.call(rbind, C_list)
  A = vapply(T_all, t_area, FUN.VALUE = double(1))
  c(weighted.mean(C[, 1], A), weighted.mean(C[, 2], A))
}

poly_centroid(poly_mat)

poly_centroid_sfg = function(x) {
  centroid_coords = poly_centroid(x)
  sf::st_point(centroid_coords)
}

poly_sfc = sf::st_polygon(list(poly_mat))
identical(poly_centroid_sfg(poly_mat), sf::st_centroid(poly_sfc))

















































