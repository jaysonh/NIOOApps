varying vec3 norm;

void main(void)
{
	gl_FragColor = vec4( norm.z, 1.0-norm.x, norm.x, 1.);
}

