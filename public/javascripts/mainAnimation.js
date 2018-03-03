//main animation


GLOBAL.buttonDropDown = GLOBAL.getButtonDropDownObject('div.buttonContainer');

GLOBAL.contentFadeIn = GLOBAL.getFadeInObject('contentInclude');

GLOBAL.contentExpand = {
	setup : function ( widthExpandDuration, delayBetweenWidthAndHeight, heightExpandDuration ) {
		this.widthExpandDuration = widthExpandDuration;
		this.delayBetweenWidthAndHeight = delayBetweenWidthAndHeight;
		this.heightExpandDuration = heightExpandDuration
		
    	var content = $('content');
		this.endWidth = content.getStyle('width');
		this.endHeight = content.getStyle('height');;
		
		//to adjust for IE problem
		
		var contentInclude = $('contentInclude');
		contentInclude.setStyle('display', 'none');
		
		
		this.addContentBackIn = function() {
			contentInclude.setStyle('display', 'block');
		};
		
		
		//alert("content object: " + content + " width: " + this.endWidth + " height: " + this.endHeight);
	
		this.stretchWidth = new Fx.Morph(content, { 
			'transition' : 'quad:out' ,
			'duration' : this.widthExpandDuration
		});
		
		this.stretchHeight = new Fx.Tween(content, { 
			'transition' : 'quad:out' ,
			'duration' : this.heightExpandDuration,
			'units' : 'px'
		});
	
		this.stretchWidth.set( {
			'width': 0,
			'opacity' : 0.0
		});
		
		this.stretchHeight.set( 'height', 10 );
		
		//alert("height after setting to 10: " + content.getStyle('height'));
		this.totalTimeSpent = this.widthExpandDuration + this.delayBetweenWidthAndHeight + this.heightExpandDuration;
		return this.totalTimeSpent;
	},
  
  	start: function () {
		this.contentInclude
		this.stretchWidth.start({
			'width' : this.endWidth,
			'opacity' : 1.0 
		});
		
		this.stretchHeight.start.delay(this.widthExpandDuration + this.delayBetweenWidthAndHeight, 
									   this.stretchHeight, 
									   [ 'height', this.endHeight ]);
		this.addContentBackIn.delay( this.totalTimeSpent, this.addContentBackIn );
  	}
};


GLOBAL.topBarFadeIn = GLOBAL.getFadeInObject('topBar');


GLOBAL.openingAnimation = {
	setup: function(buttonFadeInDuration, 
					 pauseAfterFade, 
					 buttonFallDuration, 
					 buttonBetweenPause, 
					 beforeWidthExpand, 
					 widthExpandLength, 
					 beforeHeightExpand, 
					 heightExpandLength, 
					 waitForTopBar, 
					 topBarFadeInDuration,
					 beforeContentFadeIn,
					 contentFadeInDuration) {
		this.waitForTopBar = waitForTopBar;
		
		var buttonTime = GLOBAL.buttonDropDown.setup( buttonFadeInDuration, pauseAfterFade, buttonFallDuration, buttonBetweenPause );
			
		this.waitForExpand = 0;//buttonTime + beforeWidthExpand;	// making the button and content fade in animations happening concurrently
		var contentExpandTime = GLOBAL.contentExpand.setup( widthExpandLength, beforeHeightExpand, heightExpandLength);
		this.timeToContentFadeIn = this.waitForExpand + contentExpandTime + beforeContentFadeIn;
		var contentFadeInTime = GLOBAL.contentFadeIn.setup( contentFadeInDuration );
		this.totalSetupTime = this.timeToContentFadeIn + contentFadeInDuration; 
		
		GLOBAL.topBarFadeIn.setup(topBarFadeInDuration);   //disregard the time spent for the top bar fade in
		
		return this.totalSetupTime;
	},
	start: function() {
		
		GLOBAL.buttonDropDown.start();
		GLOBAL.contentExpand.start.delay( this.waitForExpand, GLOBAL.contentExpand);
		GLOBAL.contentFadeIn.start.delay( this.timeToContentFadeIn, GLOBAL.contentFadeIn);
		GLOBAL.topBarFadeIn.start.delay( this.totalSetupTime + this.waitForTopBar, GLOBAL.topBarFadeIn);
	}
};

