
--The first question is converting  R G B Float input returning (H, S, V) Tuple
maxRGB :: Float -> Float -> Float -> Float
maxRGB x y z
   | x>=y && x>=z = x
   | y>=z = y
   | otherwise = z

minRGB :: Float -> Float -> Float -> Float
minRGB x y z
   | x<=y && x<=z = x
   | y<=z = y
   | otherwise = z

fMod :: Float -> Float -> Float
fMod x y
   | x < y = x
   | otherwise = fMod (x-y) y

calh :: (Float,Float,Float,Float,Float)-> Float
calh (rnot, gnot, bnot, delta, cmax) 
   | delta == 0 = 0
   | cmax == rnot = 60* fMod ((gnot - bnot)/ delta) 6 
   | cmax == gnot = 60* ((bnot - rnot)/ delta +2 )
   | cmax == bnot = 60* ((rnot - gnot)/ delta +4)
   
satCalculation :: Float->Float->Float
satCalculation cmax delta
   |cmax==0 = 0
   |otherwise = delta/cmax

rgb2hsv :: Float->Float->Float->(Float,Float,Float)
rgb2hsv r g b = (hue,saturation,value)
   where
      rnot = r/255
      gnot = g/255
      bnot = b/255
      cmax = maxRGB rnot gnot bnot
      cmin = minRGB rnot gnot bnot
      delta = cmax - cmin
      hue = calh (rnot, gnot, bnot, delta, cmax)
      saturation = satCalculation cmax delta
      value = cmax
--Second question is converting H, S, V Float input returning to (R, G, B) Tuple
hsvCal::Float -> Float->Float->(Float,Float,Float)
hsvCal h s v = (c,x,m)
   where
    c=v*s
    x=c*(1.0-abs((fMod (h/60) 2.0)-1.0))
    m=v-c

rgbNotFounder :: Float -> Float->Float->(Float,Float,Float)
rgbNotFounder h s v
  | h >=0.0   && h<60.0   = (c,x,0.0)
  | h >=60.0  && h<120.0  = (x,c,0.0)
  | h >=120.0 && h<180.0  = (0.0,c,x)
  | h >=180.0 && h<240.0 = (0.0,x,c)
  | h >=240.0 && h<300.0  = (x,0.0,c)
  | h >=300.0 && h<360.0  = (c,0.0,x)
  |otherwise     = error "negative parameter"
  where
   (c,x,m)=hsvCal h s v

hsv2rgb :: Float->Float->Float->(Float,Float,Float)
hsv2rgb h s v 
   | h >= 0  && h < 360&& s >= 0 && s <= 1.0 && v >= 0 && v <= 1.0 = (r,g,b)
   | otherwise    = error "Not valid input" 
    where
     (rnot,gnot,bnot) = rgbNotFounder h s v
     (c,x,m) = hsvCal h s v 
     r = abs((rnot+m)*255.0)
     g = abs((gnot+m)*255.0)
     b = abs((bnot+m)*255.0)
--Third question is converting Color Name as a String input to Float (R, G, B) Tuple
name2rgb :: String -> (Float,Float,Float)
name2rgb s
   |s=="AliceBlue" = (240,248,255)
   |s=="AntiqueWhite" = (250,235,215)
   |s=="Aqua" = (0,255,255)
   |s=="Aquamarine" = (127,255,212)
   |s=="Azure" = (240,255,255)
   |s=="Beige" = (245,245,220)
   |s=="Bisque" = (255,228,196)
   |s=="Black" = (0,0,0)
   |s=="BlanchedAlmond" = (255,235,205)
   |s=="Blue" = (0,0,255)
   |s=="BlueViolet" = (138,43,226)
   |s=="Brown" = (165,42,42)
   |s=="BurlyWood" = (222,184,135)
   |s=="CadetBlue" = (95,158,160)
   |s=="Chartreuse" = (127,255,0)
   |s=="Chocolate" = ( 210,105,30)
   |s=="Coral" = (255,127,80)
   |s=="CornflowerBlue"  = (100,149,237)
   |s=="Cornsilk" = (255,248,220)
   |s=="Crimson"  = (220,20,60)
   |s=="Cyan" = (0,255,255)
   |s=="DarkBlue" = (0,0,139)
   |s=="DarkCyan" = (0,139,139)
   |s=="DarkGoldenRod" = (184,134,11)
   |s=="DarkGray" = (169,169,169)
   |s=="DarkGrey" = (169,169,169)
   |s=="DarkGreen" = (0,100,0)
   |s=="DarkKhaki" = (189,183,107)
   |s=="DarkMagenta" = (139,0,139)
   |s=="DarkOliveGreen" = (85,107,47)
   |s=="DarkOrange"  = (255,140,0)
   |s=="DarkOrchid" = (153,50,204)
   |s=="DarkRed" = (139,0,0)
   |s=="DarkSalmon" = (233,150,122)
   |s=="DarkSeaGreen" = (143,188,143)
   |s=="DarkSlateBlue" = (72,61,139)
   |s=="DarkSlateGray" = (47,79,79)
   |s=="DarkSlateGrey" = (47,79,79)
   |s=="DarkTurquoise" = (0,206,209)
   |s=="DarkViolet" = (148,0,211)
   |s=="DeepPink" = (255,20,147)
   |s=="DeepSkyBlue" = (0,191,255)
   |s=="DimGray" = (105,105,105)
   |s=="DimGrey" = (105,105,105)
   |s=="DodgerBlue" = (30,144,255)
   |s=="FireBrick" = (178,34,34)
   |s=="FloralWhite" = (255,250,240)
   |s=="ForestGreen" = (34,139,34)
   |s=="Fuchsia" = (255,0,255)
   |s=="Gainsboro" = (220,220,220)
   |s=="GhostWhite" = (248,248,255)
   |s=="Gold" = (255,215,0)
   |s=="GoldenRod" = (218,165,32)
   |s=="Gray" = (128,128,128)
   |s=="Grey" = (128,128,128)
   |s=="Green" = (0,128,0)
   |s=="GreenYellow" = (173,255,47)
   |s=="HoneyDew" = (240,255,240)
   |s=="HotPink" = (255,105,180)
   |s=="IndianRed" = (205,92,92)
   |s=="Indigo" = (75,0,130)
   |s=="Ivory" = (255,255,240)
   |s=="Khaki" = (240,230,140)
   |s=="Lavender" = (230,230,250)
   |s=="LavenderBlush" = (255,240,245)
   |s=="LawnGreen" = (124,252,0)
   |s=="LemonChiffon" = (255,250,205)
   |s=="LightBlue" = (173,216,230)
   |s=="LightCoral" = (240,128,128)
   |s=="LightCyan" = (224,255,255)
   |s=="LightGoldenRodYellow"  = (250,250,210)
   |s=="LightGray" = (211,211,211)
   |s=="LightGrey" = (211,211,211)
   |s=="LightGreen" = (144,238,144)
   |s=="LightPink" = (255,182,193)
   |s=="LightSalmon" = (255,160,122)
   |s=="LightSeaGreen" = (32,178,170)
   |s=="LightSkyBlue" = (135,206,250)
   |s=="LightSlateGray" = (119,136,153)
   |s=="LightSlateGrey" = (119,136,153)
   |s=="LightSteelBlue" = (176,196,222)
   |s=="LightYellow" = (255,255,224)
   |s=="Lime" = (0,255,0)
   |s=="LimeGreen" = (50,205,50)
   |s=="Linen" = (250,240,230)
   |s=="Magenta" = (255,0,255)
   |s=="Maroon" = (128,0,0)
   |s=="MediumAquaMarine"  = (102,205,170)
   |s=="MediumBlue" = (0,0,205)
   |s=="MediumOrchid" = (186,85,211)
   |s=="MediumPurple" = (147,112,219)
   |s=="MediumSeaGreen" = (60,179,113)
   |s=="MediumSlateBlue" = (123,104,238)
   |s=="MediumSpringGreen" = (0,250,154)
   |s=="MediumTurquoise" = (72,209,204)
   |s=="MediumVioletRed" = (199,21,133)
   |s=="MidnightBlue" = (25,25,112)
   |s=="MintCream" = (245,255,250)
   |s=="MistyRose" = (255,228,225)
   |s=="Moccasin" = (255,228,181)
   |s=="NavajoWhite" = (255,222,173)
   |s=="Navy" = (0,0,128)
   |s=="OldLace" = (253,245,230)
   |s=="Olive" = (128,128,0)
   |s=="OliveDrab" = (107,142,35)
   |s=="Orange" = (255,165,0)
   |s=="OrangeRed" = (255,69,0)
   |s=="Orchid" = (218,112,214)
   |s=="PaleGoldenRod" = (238,232,170)
   |s=="PaleGreen" = (152,251,152)
   |s=="PaleTurquoise"  = (175,238,238)
   |s=="PaleVioletRed" = (219,112,147)
   |s=="PapayaWhip" = (255,239,213)
   |s=="PeachPuff" = (255,218,185)
   |s=="Peru" = (205,133,63)
   |s=="Pink" = (255,192,203)
   |s=="Plum" = (221,160,221)
   |s=="PowderBlue" = (176,224,230)
   |s=="Purple" = (128,0,128)
   |s=="RebeccaPurple" = (102,51,153)
   |s=="Red" = (255,0,0)
   |s=="RosyBrown" = (188,143,143)
   |s=="RoyalBlue" = (65,105,225)
   |s=="SaddleBrown" = (139,69,19)
   |s=="Salmon" = (250,128,114)
   |s=="SandyBrown" = (244,164,96)
   |s=="SeaGreen" = (46,139,87)
   |s=="SeaShell" = (255,245,238)
   |s=="Sienna" = (160,82,45)
   |s=="Silver" = (192,192,192)
   |s=="SkyBlue" = (135,206,235)
   |s=="SlateBlue" = (106,90,205)
   |s=="SlateGray" = (112,128,144)
   |s=="SlateGrey" = (112,128,144)
   |s=="Snow" = (255,250,250)
   |s=="SpringGreen" = (0,255,127)
   |s=="SteelBlue" = (70,130,180)
   |s=="Tan" = (210,180,140)
   |s=="Teal" = (0,128,128)
   |s=="Thistle" = (216,191,216)
   |s=="Tomato" = (255,99,71)
   |s=="Turquoise" = (64,224,208)
   |s=="Violet" = (238,130,238)
   |s=="Wheat" = (245,222,179)
   |s=="White" = (255,255,255)
   |s=="WhiteSmoke" = (245,245,245)
   |s=="Yellow" = (255,255,0)
   |s=="YellowGreen"  = (154,205,50)
   |otherwise = error "Not valid name "
-- 4th question is about HSV Gradient Step
rgb2hsv2 :: (Float,Float,Float)->(Float,Float,Float)
rgb2hsv2 (r, g, b) = (hue,saturation,value)
   where
      rnot = r/255
      gnot = g/255
      bnot = b/255
      cmax = maxRGB rnot gnot bnot
      cmin = minRGB rnot gnot bnot
      delta = cmax - cmin
      hue = calh (rnot, gnot, bnot, delta, cmax)
      saturation = satCalculation cmax delta
      value = cmax
findGradients :: (Float,Float,Float) -> (Float,Float,Float) -> Float -> [(Float,Float,Float)]
findGradients (r,g,b) (rd,gd,bd) steps
   | steps == 0 = [(r,g,b)]
   | otherwise = [(r+x*rd,g+x*gd,b+x*bd)| x<-[0..steps]]

hsvGradient::(Float,Float,Float)->(Float,Float,Float)->Float->[(Float,Float,Float)]
hsvGradient (h1,s1,v1) (h2,s2,v2) steps = [rgb2hsv2  x|x<-result]
   where
    (r1,g1,b1)   = hsv2rgb h1 s1 v1
    (r2,g2,b2)   = hsv2rgb h2 s2 v2
    rdelta = (r2-r1)/steps
    gdelta = (g2-g1)/steps
    bdelta = (b2-b1)/steps
    result  = findGradients (r1,g1,b1) (rdelta,gdelta,bdelta) steps



-- 5th qeestion is H S V Float input is converting description of Human Readable Color ( String, String, String)
hueName::Float -> String
hueName h 
    |h <  1            = "red"
    |h == 15           = "reddish"
    |h >  15 && h<45   = "orange"
    |h >  45 && h<70   = "yellow"
    |h >  70 && h<79   = "lime"
    |h >  79 && h<163  = "green"
    |h > 163 && h<193  = "cyan"
    |h > 193 && h<240  = "blue" 
    |h > 240 && h<260  = "indigo"
    |h > 260 && h<270  = "violet"
    |h > 270 && h<291  = "purple"
    |h > 291 && h<327  = "magenta"
    |h > 327 && h<344  = "rose"
    |h > 344           = "red"
    |otherwise = error "Not Valid Hue Value"

saturationName::Float->String
saturationName s
   |s > 0.03  && s<= 0.04 = "almost grey"
   |s > 0.04  && s<= 0.10 = "grey"
   |s > 0.10  && s<= 0.30 = "very unsaturated"
   |s > 0.30  && s<= 0.46 = "unsaturated"
   |s > 0.46  && s<= 0.60 = "rather unsaturated"
   |s > 0.60  && s<= 0.80 = "saturated"
   |s > 0.80  && s<= 0.90 = "rather saturated"
   |s > 0.90  && s<=1.0   = "very saturated"
   |otherwise = error "Not Valid Saturation Value"

 
lightnessName::Float->String
lightnessName l
   |l < 0.09                  = "almost black"    
   |l > 0.09      && l<=0.10 = "very dark"  
   |l > 0.22      && l<=0.30  = "dark"
   |l > 0.30      && l<=0.60  = "normal"
   |l > 0.60      && l<=0.80  = "light"
   |l > 0.80      && l<=0.94  = "very light"
   |l > 0.94                  = "almost white"
   |otherwise = error "Not Valid HSV Value"
calculateLight::Float->Float->Float
calculateLight cmax cmin = l
   where
     l= (cmax+cmin)/2
 
hsv2desc::Float->Float->Float->(String,String,String)
hsv2desc h s v = (hueDef,satDef,valDef)
  where
    hueDef = hueName h 
    hsls = (s*v) / (if light < 0.5 then light*2 else 2-light*2)
    satDef = saturationName hsls
    light  =  (2-s) * v / 2
    valDef = lightnessName light
--6th question is about
namehsv::String->String->Float->[(Float,Float,Float)]
namehsv name1 name2 steps = hsvGradient clrHSV1 clrHSV2 steps
   where
    clrRGB1 = name2rgb name1
    clrRGB2 = name2rgb name2
    clrHSV1 = rgb2hsv2 clrRGB1
    clrHSV2 = rgb2hsv2 clrRGB2
    
    