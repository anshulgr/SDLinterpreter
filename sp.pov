sphere {
 <0,0,1.5>,
  1
 pigment { color rgb <1,0,0> } 
 }
camera {
sky <0,0,1>  
up    <0,1,0>         
direction <-1,0,0>      
right <-1.25,0,0>      
location <30,10,1.5> 
look_at <0,0,0>
angle 15
}
light_source {
  <10,-10,20>   
  color rgb <1,1,1>
}
