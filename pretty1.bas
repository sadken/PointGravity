#Include "fbgfx.bi"

Using FB

Randomize Timer

Type particle 
	x As Double
	y As Double
	z As Double
	x_vel As Double
	y_vel As Double
	x_acc As Double
	y_acc As Double
	x_force As Double
	y_force As Double
	mass As ULongInt
	r As UByte
	g As UByte
	b As UByte
	alive As Byte
End Type

Type pythag
	opp As Double
	adj As Double
	hyp As Double
	angle As Double
End Type

Type gravity_point 
	x As Double
	y As Double
	z As Double
	mass As ULongint
	radius As Double
End Type

Declare Sub init_particle(max As Integer)
Declare Function draw_particle(max As Integer, dot As UByte) As Integer
Declare Sub move_particle(gpoint As gravity_point,max As Integer, dt As Double)
Declare Function calc_Gforce(m1 As Double, m2 As Double, dist As Double) As Double
Declare Function calc_pythag(x1 As Double, x2 As Double, y1 As Double, y2 As Double) As pythag

Const max_particle = 100000
Const Screen_width = 1280
Const screen_height = 720
Const TRUE = 1
Const FALSE = 0
Const FIXED_TIME_STEP = 1/60
Const Gravity_const = .0000000000667834
Const max_force = 10
Const max_acc = 50

Dim Shared As Single init_vel, particle_mass

Const pi = 3.14159
 

Dim Shared particle(max_particle) As particle
Dim gravity_point As gravity_point

Dim As UByte fin, no_key, mse, dot
Dim Shared recycle As UByte
Dim As Double CurrentTime, LastTime, dt, angle
Dim As Integer speed , mx, my, mb, frame,i,r,g,b
Dim As Integer pCount

ScreenRes screen_width,screen_height,32,2

ScreenSet 1,0

fin = FALSE
no_key = TRUE
mse = FALSE
recycle = TRUE
dot = TRUE

dt = 0.5
speed = 10
init_vel = 5
particle_mass = 100
frame = 0

gravity_point.x = 300
gravity_point.y = 300
gravity_point.z = 0
gravity_point.mass = 100000000000
gravity_point.radius = 20


init_particle(max_particle)

While fin = FALSE

   LastTime = CurrentTime
   CurrentTime = Timer
   dt = CurrentTime - LastTime
   'If( dt > FIXED_TIME_STEP ) then dt = FIXED_TIME_STEP
   
   GetMouse(mx, my, ,mb)
   If mse = TRUE Then 
   	gravity_point.x = mx
   	gravity_point.y = my
   EndIf
   If Not MultiKey(SC_D) And Not MultiKey(SC_E) And Not MultiKey(SC_M) And Not MultiKey(SC_UP) And Not MultiKey(SC_DOWN) And Not MultiKey(SC_LEFT) And Not MultiKey(SC_RIGHT) And Not MultiKey(SC_EQUALS) And Not MultiKey(SC_MINUS) Then no_key = TRUE
   
	If MultiKey(SC_ESCAPE) Then fin = TRUE
	If MultiKey(SC_D) And no_key = TRUE Then 
		If dot = TRUE Then dot = FALSE Else dot = TRUE
		no_key = FALSE
	EndIf
	If MultiKey(SC_E) And no_key = TRUE Then 
		If recycle = TRUE Then recycle = FALSE Else recycle = TRUE
		no_key = FALSE
	EndIf
	If MultiKey(SC_R) Then 
		init_particle(max_particle)
		frame = 0
	EndIf
	If MultiKey(SC_UP) And no_key = TRUE Then 
		If speed >=10 Then 
			speed += 10
		Else
			speed += 1
		EndIf
		no_key = FALSE
	EndIf
	If MultiKey(SC_DOWN) And no_key = TRUE Then 
		If speed > 10 Then 
			speed += -10
		Else
			speed += -1
		EndIf
		no_key = FALSE
	EndIf
	
	If MultiKey(SC_LEFT) And no_key = TRUE Then 
		init_vel += -1
		no_key = FALSE
	EndIf
	If MultiKey(SC_RIGHT) And no_key = TRUE Then 
		init_vel += 1
		no_key = FALSE
	EndIf
	
	If MultiKey(SC_EQUALS) And no_key = TRUE Then 
		particle_mass += 10
		no_key = FALSE
	EndIf
	If MultiKey(SC_MINUS) And no_key = TRUE Then 
		particle_mass += -10
		no_key = FALSE
	EndIf
	If MultiKey(SC_M) And no_key = TRUE Then 
		If mse = TRUE Then mse = FALSE Else mse = TRUE
		no_key = FALSE
	EndIf
	If MultiKey(SC_B) Then 
		For i = 0 To max_particle
			angle = pi-(Rnd*pi*2)
			particle(i).x_vel = Cos(angle) * init_vel
			particle(i).y_vel = Sin(angle) * init_vel
			particle(i).x = screen_width/2
			particle(i).y = screen_height/2
			particle(i).mass = particle_mass
			particle(i).alive = TRUE
		Next
		frame = max_particle
	EndIf
	
	If speed < 0 Then speed = 0
	If speed > 50 Then speed = 50
	If init_vel < 0 Then init_vel = 0
	If particle_mass < 0 Then particle_mass = 0
	
	If frame < max_particle Then 
		angle = pi-(Rnd*pi*2)
		particle(frame).x_vel = Cos(angle) * init_vel
		particle(frame).y_vel = Sin(angle) * init_vel
		particle(frame).x = screen_width/2
		particle(frame).y = screen_height/2
		particle(frame).mass = particle_mass
		particle(frame).alive = TRUE
		frame += 1
	EndIf
	move_particle(gravity_point,max_particle,dt * (speed/10))
	
	Cls
	Locate 1,1 
	Print Int(1/dt) & " FPS"
	Locate 5,45
	Print "Press Escape to Exit"
	Locate 2,2
	Print "       Run Speed: x"&speed/10
	Locate 2,45
	Print  "(Up/Down Arrow keys adjust speed)"
	Locate 3,2 
	Print "  Particle Speed: "&init_vel
	Locate 3,45 
	Print "(Left/Right Arrow keys adjust initial speed)"
	Locate 4,2 
	Print "   Particle Mass: " & particle_mass
	Locate 4,45
	Print "(+/- Keys adjust particle mass)"
	Locate 5,2 
	Print "Active Particles: "& pcount & " Of " & max_particle
	Locate 6,45
	Print "Press B to reset with a burst!"
	Locate 6,2
	If recycle = TRUE Then Print "Particle Recycle: ON" Else Print "Particle Recycle: OFF"
	Locate 7,2
	If mse = TRUE Then Print "      Mouse Mode: ON" Else Print "      Mouse Mode: OFF"
	Locate 7,45
	Print "Press M to activate mouse mode (moves gravity point with mouse)"
	Locate 8,2
	If dot=TRUE Then Print "  Draw Particles: Points" Else Print "  Draw Particles: Dots"
	Locate 8,45
	Print "Press D to toggle particle drawing points or dots"
	
	Circle (gravity_point.x, gravity_point.y),gravity_point.radius,RGB(128,128,128),,,,F
	pCount = draw_particle(frame,dot)
	For i = gravity_point.radius - 1 To 0 Step -1
		r = (i/gravity_point.radius)*32
		g = (i/gravity_point.radius)*32
		b = (i/gravity_point.radius)*32
		Circle (gravity_point.x, gravity_point.y),i,RGB(r,g,b),,,,F
	Next
	'ScreenSync
	ScreenCopy
	
Wend

End 0

Sub init_particle(max As Integer)
	
	Dim i As Integer, angle As Double
	
	For i = 0 To max
			
		particle(i).x = screen_width/2
		particle(i).y = screen_height/2
		particle(i).z = ((i/max)/2)+0.5
		particle(i).x_vel = 0
		particle(i).y_vel = 0

		particle(i).x_force = 0
		particle(i).y_force = 0
		
		particle(i).mass = particle_mass
		
		particle(i).alive = FALSE
		
		particle(i).x_acc = 0
		particle(i).y_acc = 0
		
		particle(i).r = 255
		particle(i).g = 128
		particle(i).b = 64
	Next
End Sub

Function draw_particle(max As Integer, dot As UByte) As Integer
	
	Dim As Integer i, r, g, b, count
	
	count = 0
	
	If dot=TRUE Then  
		For i = 0 To max
			If particle(i).alive = TRUE Then
				count +=1
				If particle(i).x < screen_width And particle(i).x > 0 Then
					If particle(i).y < screen_height And particle(i).y > 0 Then
						r = particle(i).r * particle(i).z
						If r < 0 Then r = 0
						g = particle(i).g * particle(i).z
						If g < 0 Then g = 0
						b = particle(i).b * particle(i).z
						If b < 0 Then b = 0
						PSet (particle(i).x, particle(i).y), RGB(r,g,b)
						
					EndIf				
				EndIf
			EndIf
		Next
	Else
		For i = 0 To max
			If particle(i).alive = TRUE Then
				count += 1
				If particle(i).x < screen_width And particle(i).x > 0 Then
					If particle(i).y < screen_height And particle(i).y > 0 Then
					 
						r = particle(i).r * particle(i).z
						If r < 0 Then r = 0
						g = particle(i).g * particle(i).z
						If g < 0 Then g = 0
						b = particle(i).b * particle(i).z
						If b < 0 Then b = 0
				
						'PSet (particle(i).x, particle(i).y), RGB(r,g,b)
						Circle (particle(i).x, particle(i).y),2,RGB(r,g,b),,,,F
						
					EndIf
				EndIf
			EndIf
		Next
	EndIf
	
	If count > max Then count = max
	
	Return count
	
End Function

Sub move_particle(gpoint As gravity_point, max As Integer, dt As Double)
	
	Dim i As Integer, p As pythag, force As Double, acc As Double, angle As Double
	
	For i = 0 To max
		If particle(i).alive = TRUE Then 
			p = calc_pythag(gpoint.x,particle(i).x,gpoint.y,particle(i).y)
			
			force = calc_Gforce(gpoint.mass, particle(i).mass,p.hyp)
			acc = force * particle(i).mass
			
			If p.hyp < gpoint.radius And recycle = TRUE Then 
				particle(i).x = screen_width/2
				particle(i).y = screen_height/2
				particle(i).x_acc = 0
				particle(i).y_acc = 0
				angle = pi-(Rnd*pi*2)
				particle(i).x_vel = Cos(angle) * init_vel
				particle(i).y_vel = Sin(angle) * init_vel
				particle(i).mass = particle_mass
				force = 0
				acc = 0
			EndIf
			If p.hyp < gpoint.radius And recycle = FALSE Then 
				particle(i).x = -1
				particle(i).y = -1
				particle(i).x_acc = 0
				particle(i).y_acc = 0
				particle(i).x_vel = 0
				particle(i).y_vel = 0
				particle(i).mass = 0
				particle(i).alive = FALSE
				force = 0
				acc = 0
			EndIf
			
			particle(i).x_acc = Cos(p.angle) * acc
			particle(i).y_acc = Sin(p.angle) * acc
							
			particle(i).x_vel += particle(i).x_acc * dt
			particle(i).y_vel += particle(i).y_acc * dt
			
			particle(i).x += particle(i).x_vel * dt
			particle(i).y += particle(i).y_vel * dt
				
			If (particle(i).x > screen_width Or particle(i).x < 0 Or particle(i).y > screen_height Or particle(i).y < 0) And recycle = TRUE Then 
				particle(i).x = screen_width/2
				particle(i).y = screen_height/2
				particle(i).x_acc = 0
				particle(i).y_acc = 0
				angle = pi-(Rnd*pi*2)
				particle(i).x_vel = Cos(angle) * init_vel
				particle(i).y_vel = Sin(angle) * init_vel
				particle(i).mass = particle_mass
				particle(i).alive = TRUE
			EndIf
			
			If Abs(particle(i).x) > screen_width * 2 Or Abs(particle(i).y) > screen_height * 2 And recycle = FALSE Then particle(i).alive = FALSE
		EndIf
	Next
End Sub

Function calc_Gforce(m1 As Double, m2 As Double, dist As Double) As Double
	
	Dim force As Double
	
	force = Gravity_const * ((m1 * m2)/(dist*dist))
	
	Return force
	
End Function

Function calc_pythag(x1 As Double, x2 As Double, y1 As Double, y2 As Double) As pythag
	
	Dim p As pythag
	
	p.opp = x1 - x2
	p.adj = y1 - y2
	p.hyp = Sqr(p.adj * p.adj + p.opp * p.opp)
	
	p.angle = ATan2(p.adj,p.opp)
	
	Return p
	
End Function
