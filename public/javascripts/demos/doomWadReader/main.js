var requestWithGET = function(url, onSuccess, onFailure) {
	var request = new XMLHttpRequest()
	request.open("GET", url)
	request.onreadystatechange = function() {
		if (request.readyState === 4) {
			if(request.status === 200) {
				onSuccess(request.getResponseHeader("Content-Type"), request.responseText);
			}
			else {
				onFailure();
			}
		}
	}
	request.send(null)
}

var requestWithPOST = function(url, data, onSuccess, onFailure) {
	var request = new XMLHttpRequest()
	request.open("POST", url)
	request.onreadystatechange = function() {
		if (request.readyState === 4) {
			if(request.status === 200) {
				onSuccess(request.getResponseHeader("Content-Type"), request.responseText);
			}
			else {
				onFailure();
			}
		}
	}
	request.send(data)
}

var displayDoomAssets = function(responseToRequest, elapsedTime, labelForDoomAssets) {
	var result = responseToRequest.result
	var parentOfImages = document.getElementById('resultsGoHere');
	while (parentOfImages.lastChild) {
		parentOfImages.removeChild(parentOfImages.lastChild);
	}
	
	if (typeof result == "string") {	// error occurred
		var errorContainer = document.createElement("div");
		errorContainer.className = "errorContainer";
		errorContainer.innerHTML = "An error was encountered while processing the data on the other end of the link you submitted! Perhaps the data wasn't a WAD file intended for Doom 1?";
		parentOfImages.appendChild(errorContainer)
	}
	else {
		var imgElementForNamedImage = function(namedImage) {
			var imgElement = document.createElement("img");
			imgElement.src = namedImage.imageSrc;
			imgElement.title = namedImage.name;
			imgElement.className = "imageFromWAD";
			return imgElement
		}
		var processSetOfImages = function(title, description, images, parentOfImages) {
			if (images.length > 0 ) {
				var container = document.createElement("div");
				container.className = "sectionContainer";
				
				var headerDivElement = document.createElement("div");
				headerDivElement.className = "header";
				container.appendChild(headerDivElement);
			
				var detailsSpanElement = document.createElement("span");
				detailsSpanElement.className = "details";
				headerDivElement.appendChild(detailsSpanElement);
				var titleSpanElement = document.createElement("span");
				titleSpanElement.className = "title";
				detailsSpanElement.appendChild(titleSpanElement);
				var descriptionSpanElement = document.createElement("span");
				descriptionSpanElement.className = "description";
				detailsSpanElement.appendChild(descriptionSpanElement);
				
				titleSpanElement.innerHTML = title;
				descriptionSpanElement.innerHTML = description;
				
				for (var i = 0; i < images.length; i++) {
					container.appendChild(imgElementForNamedImage(images[i]));
				}
				parentOfImages.appendChild(container)
				return true;
			}
			else {
				return false;
			}
		}

		var resultHeader = document.createElement("div");
		resultHeader.className = "resultHeader";
		
		var fileNameSpanElement = document.createElement("span");
		fileNameSpanElement.className = "fileName";
		fileNameSpanElement.innerHTML = labelForDoomAssets;
		resultHeader.appendChild(fileNameSpanElement);
		
		parentOfImages.appendChild(resultHeader)
		
		var spritesProcessed = processSetOfImages("sprites", "objects that appear inside a map", result.sprites, parentOfImages);
		var flatsProcessed = processSetOfImages("flats", "used on ceilings and floors", result.flats, parentOfImages);
		var texturesProcessed = processSetOfImages("textures", "used on walls", result.textures, parentOfImages);
		var otherGraphicsProcessed = processSetOfImages("other graphics", "UI elements and miscellaneous other images", result.otherGraphics, parentOfImages);
	
		if (!(flatsProcessed || spritesProcessed || texturesProcessed || otherGraphicsProcessed)) {
			// let the user know that no images were found, good chance this WAD just introduces new maps using all the original image assets from doom
			var errorContainer = document.createElement("div");
			errorContainer.className = "noImagesContainer";
			errorContainer.innerHTML = "No images were found while processing the WAD you submitted.  This is unfortunate, but not unexpected because many Doom WADs only introduce new maps using all the original image/texture assets from Doom.";
			parentOfImages.appendChild(errorContainer)
		}
	}

	var createTimingDiv = function(description, ms) {
		var timingInformation = document.createElement("div");
		timingInformation.innerHTML = description + ": " + (ms / 1000) + " seconds";
		return timingInformation;
	}

	var createTimingListItem = function(description, ms) {
		var listItem = document.createElement("li");
		listItem.appendChild(createTimingDiv(description, ms));
		return listItem;
	}

	var statsSection = document.createElement("div");
	statsSection.id = "stats";
	parentOfImages.appendChild(statsSection);
	var statsLabel = document.createElement("div");
	statsLabel.id = "label";
	statsLabel.innerHTML = "Stats";
	var statsContents = document.createElement("div");
	statsContents.id = "contents";
	statsSection.appendChild(statsLabel);
	statsSection.appendChild(statsContents);
	statsContents.appendChild(createTimingDiv("Total time seen by browser", elapsedTime));
	if (typeof responseToRequest.timings !== 'undefined') {
		var subtimingsList = document.createElement("ul");
		statsContents.appendChild(subtimingsList);
		subtimingsList.appendChild(createTimingListItem("time spent retrieving file", responseToRequest.timings.retrieveFile));
		subtimingsList.appendChild(createTimingListItem("time spent parsing file", responseToRequest.timings.parseFile));
		subtimingsList.appendChild(createTimingListItem("time spent building images", responseToRequest.timings.buildImages));
	}
}

var uriIsBeingDropped = function(jqueryDndEvent) {
	var hasUriList = -1 !== $.inArray("text/uri-list", jqueryDndEvent.originalEvent.dataTransfer.types);
	var hasUrl = -1 !== $.inArray("Url", jqueryDndEvent.originalEvent.dataTransfer.types);	// for IE: this was the value that I was seeing in IE 11
	var hasURL = -1 !== $.inArray("URL", jqueryDndEvent.originalEvent.dataTransfer.types);	// for IE: http://stackoverflow.com/a/18051912
	// sadly, it looks like (at least with IE 11) we can't just call event.originalEvent.dataTransfer.getData("URL"/"Url") here and check for a non-empty response, because that call is returning "" in onDragEnter
	return hasUriList || hasUrl || hasURL;
}

var fileIsBeingDropped = function(jqueryDndEvent) {
	var hasFiles = -1 !== $.inArray("Files", jqueryDndEvent.originalEvent.dataTransfer.types);
	return hasFiles;
}

var onDragEnter = function(event) {
	// understanding drag and drop for html is not easy....doesn't seem to be a clear way to communicate 'these are the allowed actions' and have that reflect with what the user's trying to do
	if ((uriIsBeingDropped(event) || fileIsBeingDropped(event)) && !$(event.currentTarget).hasClass("dropIsBeingProcessed")) {
		$(event.currentTarget).addClass("dropIsValid")
		event.originalEvent.dataTransfer.dropEffect = "copy"
		event.preventDefault();
	}
}

var onDragOver = function(event) {
	if ((uriIsBeingDropped(event) || fileIsBeingDropped(event)) && !$(event.currentTarget).hasClass("dropIsBeingProcessed")/* dragOver can happen even if dragEnter is ignored :( */) {
		event.originalEvent.dataTransfer.dropEffect = "copy"
		event.preventDefault();
	}
}

var onDragLeave = function(event) {
	$(event.currentTarget).removeClass("dropIsValid")
	event.preventDefault();
}

var extractLabelFromAnchorHtml = function(anchorHtml) {
	var tempDiv = document.createElement("div");
	tempDiv.innerHTML = anchorHtml;
	var anchors = tempDiv.getElementsByTagName("A");
	if (anchors.length == 1)	// there should be only anchor child
	{
		var anchorElement = anchors[0];
		if (anchorElement.children.length == 0)	// innerHTML is all text?
		{
			return anchorElement.innerHTML;
		}
	}
	
	return "";
}

var onDrop = function(loadingImage) {
	return function(event) {
		var getLabelFromDndEvent = function(event) {
			if (uriIsBeingDropped(event)) {	// links
				var uri = event.originalEvent.dataTransfer.getData("URL");	// this retrieves the first URL out of a possible list (https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Recommended_Drag_Types)
				
				var hasAnchorHTML = -1 !== $.inArray("text/html", event.originalEvent.dataTransfer.types);
				var uriLabel = hasAnchorHTML ? extractLabelFromAnchorHtml(event.originalEvent.dataTransfer.getData("text/html")) : uri;
				return uriLabel;
			} else if (fileIsBeingDropped(event)) {	// files on disk
								
				var files = event.originalEvent.dataTransfer.files; // Array of all files
				return files[0].name;
			}
		};
		
		var requestData = function(event, onLoadFinish) {
			if (uriIsBeingDropped(event)) {	// links
				var timeBeforeRequest = (new Date()).getTime();
				
				var uri = event.originalEvent.dataTransfer.getData("URL");	// this retrieves the first URL out of a possible list (https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Recommended_Drag_Types)
				
				var encodedUrl = encodeURIComponent(uri);
					// http://stackoverflow.com/questions/75980/best-practice-escape-or-encodeuri-encodeuricomponent
					// http://stackoverflow.com/questions/332872/encode-url-in-javascript
				
				var getAssetsLabelFromResponse = function(response) {
					if (response.result.fileName !== "") {
						return "<a href=\"" + uri + "\">" + response.result.fileName + "</a>";
					}
					else {
						// extract the end part of the ULI
						var regex = /.*[/]([^/]*)$/;
						var found = uri.match(regex);
						return found[1];
					}
				}
				
				requestWithGET("/extractDoomWadData?url=" + encodedUrl,
					function(responseType, responseText) {
						onLoadFinish();
						if (responseType == "application/json") {
							var timeAfterRequest = (new Date()).getTime();
							var response = JSON.parse(responseText);
							displayDoomAssets(response, timeAfterRequest - timeBeforeRequest, getAssetsLabelFromResponse(response));
						}
					},
					function() {
						onLoadFinish();
						var timeAfterRequest = (new Date()).getTime();
						displayDoomAssets({result: "Error on server side"}, timeAfterRequest - timeBeforeRequest);
					});
				
			} else if (fileIsBeingDropped(event)) {	// files on disk
				var timeBeforeRequest = (new Date()).getTime();
								
				var files = event.originalEvent.dataTransfer.files; // Array of all files

				var getAssetsLabelFromResponse = function(response) {
					if (response.result.fileName !== "") {
						return response.result.fileName;
					}
					else {
						return files[0].name
					}
				}
				
				requestWithPOST("/extractDoomWadData",
					files[0],
					function(responseType, responseText) {
						onLoadFinish();
						if (responseType == "application/json") {
							var timeAfterRequest = (new Date()).getTime();
							var response = JSON.parse(responseText);
							displayDoomAssets(response, timeAfterRequest - timeBeforeRequest, getAssetsLabelFromResponse(response));
						}
					},
					function(responseType, responseText) {
						onLoadFinish();
						var timeAfterRequest = (new Date()).getTime();
						displayDoomAssets({result: "Error on server side"}, timeAfterRequest - timeBeforeRequest, files[0].name);
					});
			}
		};
		
		var uriLabel = getLabelFromDndEvent(event);
		
		var targetOfDrop = event.currentTarget;
	
		$(targetOfDrop).removeClass("dropIsValid")
		$(targetOfDrop).addClass("dropIsBeingProcessed")
		
		var maxLengthForUriLabel = 30;
		var labelForLoading = "Processing \"" + uriLabel.substring(0, maxLengthForUriLabel) + (uriLabel.length > maxLengthForUriLabel ? "..." : "") + "\" ...";
		
		var dropAreaContentsWhileLoading = document.createElement("div");
		dropAreaContentsWhileLoading.className = "contents";
		var loadIcon = document.createElement("img");
		loadIcon.className = "icon";
		loadIcon.id = "loadIcon";
		loadIcon.src = loadingImage;
		var loadLabel = document.createElement("div");
		loadLabel.id = "label";
		
		var firstLineSpan = document.createElement("span");
		firstLineSpan.innerHTML = labelForLoading;
		var secondLineSpan = document.createElement("span");
		secondLineSpan.innerHTML = "You've been waiting ";
		var waitReportingSpan = document.createElement("span");
		waitReportingSpan.innerHTML = "0 seconds";
		loadLabel.appendChild(firstLineSpan);
		loadLabel.appendChild(document.createElement("br"));
		loadLabel.appendChild(secondLineSpan);
		loadLabel.appendChild(waitReportingSpan);
		
		dropAreaContentsWhileLoading.appendChild(loadIcon);
		dropAreaContentsWhileLoading.appendChild(loadLabel);
		
		var removedContents = $(targetOfDrop).children(".inner").children(".contents").replaceWith(dropAreaContentsWhileLoading)
		
		var timeAtRequest = (new Date()).getTime();
		
		var timerId = setInterval(function() {
			var currentTime = (new Date()).getTime();
			var secondsSpentWaiting = Math.round((currentTime - timeAtRequest) / 1000);
			waitReportingSpan.innerHTML = secondsSpentWaiting + " second" + (secondsSpentWaiting == 1 ? "" : "s");
		}, 500)
		
		// change the drop area to reflect that we're waiting for the load to finish
		
		var onLoadFinish = function() {
			$(targetOfDrop).children(".inner").children(".contents").replaceWith(removedContents);
			$(targetOfDrop).removeClass("dropIsBeingProcessed");
			clearTimeout(timerId);
		}
	
		event.preventDefault();
		
		requestData(event, onLoadFinish);
	}
}