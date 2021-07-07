import * as THREE from './three.module.js';
import { TWEEN } from './Tween.module.min.js';
import { TrackballControls } from './TrackballControls.js';
import { CSS3DRenderer, CSS3DObject } from './CSS3DRenderer.js'


function initGrid(elements) {
    console.log(elements);
    camera = new THREE.PerspectiveCamera( 40, window.innerWidth / window.innerHeight, 1, 10000 );
    camera.position.z = 3000;

    scene = new THREE.Scene();
    var targets = {grid : []}
    var objects = []
    // grid

    for ( var i = 0; i < elements.length; i ++ ) {
        var cssObj = new THREE.CSS3DObject(elements[i]);
        cssObj.position.x = Math.random() * 4000 - 2000;
				cssObj.position.y = Math.random() * 4000 - 2000;
				cssObj.position.z = Math.random() * 4000 - 2000;
				scene.add( cssObj );
        objects.push(cssObj);
        
        var object = new THREE.Object3D();
        object.position.x = ( ( i % 5 ) * 400 ) - 800;
        object.position.y = ( - ( Math.floor( i / 5 ) % 5 ) * 400 ) + 800;
        object.position.z = ( Math.floor( i / 25 ) ) * 1000 - 2000;
        targets.grid.push( object );

    }
    //

    renderer = new THREE.CSS3DRenderer();
    renderer.setSize( window.innerWidth, window.innerHeight );
    renderer.domElement.style.position = 'absolute';
    document.getElementById( 'gridContainer' ).appendChild( renderer.domElement );

    //

    controls = new THREE.TrackballControls( camera, renderer.domElement );
    controls.rotateSpeed = 0.5;
    controls.minDistance = 500;
    controls.maxDistance = 6000;
    controls.addEventListener( 'change', render );

    var button = document.getElementById( 'grid' );
    button.addEventListener( 'click', function ( event ) {

      transform( targets.grid, 2000 );

    }, false );

    transform( targets.grid, 2000, objects );

    //

    window.addEventListener( 'resize', onWindowResize, false );
} 


function transform( targets, duration, elements ) {

				TWEEN.removeAll();

				for ( var i = 0; i < elements.length; i ++ ) {

					var object = elements[ i ];
					var target = targets[ i ];

					new TWEEN.Tween( object.position )
						.to( { x: target.position.x, y: target.position.y, z: target.position.z }, Math.random() * duration + duration )
						.easing( TWEEN.Easing.Exponential.InOut )
						.start();

					new TWEEN.Tween( object.rotation )
						.to( { x: target.rotation.x, y: target.rotation.y, z: target.rotation.z }, Math.random() * duration + duration )
						.easing( TWEEN.Easing.Exponential.InOut )
						.start();

				}

				new TWEEN.Tween( this )
					.to( {}, duration * 2 )
					.onUpdate( render )
					.start();
}

function onWindowResize() {
	  camera.aspect = window.innerWidth / window.innerHeight;
	  camera.updateProjectionMatrix();

    renderer.setSize( window.innerWidth, window.innerHeight );
		render();
}

function animate() {
		requestAnimationFrame( animate );
		TWEEN.update();
		controls.update();
}

function render(scene, camera) {
		renderer.render( scene, camera );
}
