# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

#RUMUS VOLUME BANGUN RUANG

VolumeKubus<-function(sisi)
{VolumeKubus<-sisi^3
cat("Kubus Tersebut Memiliki Volume", VolumeKubus)
cat(" cm^3")
}

VolumeLimas<-function(alas,tinggi)
{VolumeLimas<- (1/3)*((1/2)*alas*tinggi)*tinggi
cat("Limas Tersebut Memiliki Volume", VolumeLimas)
cat(" cm^3")
}

VolumeSilinder<-function(jari.jari,tinggi)
{VolumeSilinder<-pi*jari.jari^2*tinggi
cat("Silinder Tabung Tersebut Memiliki Volume", VolumeSilinder)
cat(" cm^3")
}

VolumetTabung<-function(r,tinggi)
{VolumeTabung<-pi*r^2*tinggi
cat("Tabung Tersebut Memiliki Volume", volumetabung)
cat(" cm^3")}

VolumeBalok<-function(panjang,lebar,tinggi)
{VolumeBalok<-panjang*lebar*tinggi
cat("Balok Tersebut Memiliki Volume", volumebalok)
cat(" cm^3")
}

#RuMUS LUAS
LuasSegitiga = function(alas,tinggi)
{luas=(1/2)*alas*tinggi
cat("luasnya adalah :",luas)
cat(" cm^2")}

LuasPersegi = function(sisi)
{luas=sisi^2
cat("luasnya adalah :",luas)
cat(" cm^2")}

LuasPersegiPanjang = function(panjang,lebar)
{luas=panjang*lebar
cat("luasnya adalah :",luas)
cat(" cm^2")}

LuasLingkaran = function(r)
{luas=pi*r^2
cat("luasnya adalah :",luas)
cat(" cm^2")}

LuasLayangLayang = function(d1,d2)
{luas=(1/2)*d1*d2
cat("luasnya adalah :",luas)
cat(" cm^2")}

LuasJajarGenjang <- function(alas, tinggi)
{
  Luas <- alas*tinggi
  cat("Luasnya adalah :", Luas)
  cat(" cm^2")
}

LuasTrapesium <- function(atas, bawah, tinggi)
{
  Luas <- (1/2)*tinggi*(atas+bawah)
  cat("Luasnya adalah :", Luas)
  cat(" cm^2")
}

LuasBelahKetupat<- function(d1, d2)
{
  Luas <- (1/2)*d1*d2
  cat("Luasnya adalah :", Luas)
  cat(" cm^2")
}

#' Geometry package: perimeter, circumference, and area calculations
#'
#' @docType package
#' @name geometry
#'
#' @importFrom stats mean
#'

#' @importFrom stats sum

#' @importFrom stats pi
#' Calculate perimeter and area of a square
#'
#' @param s Length of a side of the square
#'
#' @return A list with perimeter and area
#' @export
square <- function(s) {
  perimeter <- 4 * s
  area <- s^2
  list(perimeter = perimeter, area = area)
}

#' Calculate perimeter and area of a triangle
#'
#' @param a Length of side a
#' @param b Length of side b
#' @param c Length of side c
#'
#' @return A list with perimeter and area
#' @export
triangle <- function(a, b, c) {
  perimeter <- a + b + c
  s <- perimeter / 2
  area <- sqrt(s * (s - a) * (s - b) * (s - c))
  list(perimeter = perimeter, area = area)
}

#' Calculate circumference and area of a circle
#'
#' @param r Radius of the circle
#'
#' @return A list with circumference and area
#' @export
circle <- function(r) {
  circumference <- 2 * pi * r
  area <- pi * r^2
  list(circumference = circumference, area = area)
}

#' Calculate perimeter and area of a rectangle
#'
#' @param p Length of the rectangle
#' @param l Width of the rectangle
#'
#' @return A list with perimeter and area
#' @export
rectangle <- function(p, l) {
  perimeter <- 2 * (p + l)
  area <- p * l
  list(perimeter = perimeter, area = area)
}
