/*
* ajaxGET --- send a GET ajax call and expect json response
* path : the RELATIVE path to send the request
* params : json object conteining GET parameters
*
* return the json response
*/
function ajaxGETjson(path, params){
	console.log("AJAX : "+path+" | "+$.param(params));
	return $.ajax({
		method: "GET",
		url: "http://localhost:8000/"+path,
		data: $.param(params),
		error: function (resultat, statut, erreur) {
			console.log("error");
			alert("Erreur lors de l'appel à "+path);
			console.log(resultat, statut, erreur);
		},
		complete: function(response){
			console.log(response.responseJSON);
		}
	});
}

/* addMsg --- add a message in the msg-box
* msg : the message to be displayed
*/
function addMsg(msg, type, hoverInOut){
	type = typeof type !== 'undefined' ? type : 'info';
	var $msgbox = $("#msg-box");
	var $playsbox = $("#plays-box");
	var $lastMsgBox = $("#last-msg-box");
	var $msg = $("<li></li>").addClass("list-group-item list-group-item-"+type).text(msg);
	var $msgClone = $msg.clone();
	if(	(type === "jaune" || type === "rouge")
		&& typeof hoverInOut !== 'undefined'){
		var $msgPlaysClone = $msg.clone();
		$msg.hover(hoverInOut);
		$msgClone.hover(hoverInOut);
		$msgPlaysClone.hover(hoverInOut);
		$playsbox.prepend($msgPlaysClone);
	}
	$msgbox.prepend($msg);
	$lastMsgBox.html($msgClone);
}


function Player(){
	this.code = 0;
	this.Name = '';
	this.color = '';
}


var Game = {
	player1 : null,
	player2 : null,
	currentPlayer : null,

	init : function(){
		addMsg("Initialisation du jeu...",'primary');
		Game.player1 = new Player();
		Game.player2 = new Player();
		currentPlayer = null;
		$(".board-row").addClass("empty-cell");
		var ajax = ajaxGETjson('init', {});
		ajax.success(function(json, statut){
			if(json.correct){
				addMsg("Jeu initialisé.","success");
				Game.playerSelection(json.players);
			}
			else{
				Game.error("Erreur lors de l'initialisation du jeu.", 1)
			}
		})
	},
	reset : function(){
		var $reset = $("#control-reset");
		var $resetButton = $reset.find("button");
		$resetButton.one("click",function(){
			$reset.addClass("hide");
			$(".board-row").empty();
			Game.init();
		});
		$reset.removeClass("hide");
	},
	playerSelection : function(playersList){
		var $playerSelection =  $("#player-selection").removeClass("hide");
		for (var i = 0; i < playersList.length; i++) {
			var playerId = playersList[i][0];
			var playerName = playersList[i][1];
			var $selectableElement = $("<p></p>")
				.attr("data-id", playerId)
				.attr("data-name", playerName)
				.addClass("selectable player")
				.text(playerName);
			$selectableElement.click(function(){
				if(Game.player1.code === 0){
					Game.player1.code = $(this).attr("data-id");
					Game.player1.name = $(this).attr("data-name");
					addMsg(Game.player1.name+" a été sélectionné(e).","success");
					addMsg("Sélectionnez le joueur 2.");
				}
				else if(Game.player2.code === 0){
					Game.player2.code = $(this).attr("data-id");
					Game.player2.name = $(this).attr("data-name");
					addMsg(Game.player2.name+" a été sélectionné(e).","success");
					$playerSelection.addClass("hide").find(".player").remove();
					addMsg("Confirmation de la sélection des joueurs...","primary");
					var ajax = ajaxGETjson('selectPlayers', {joueur1:Game.player1.code, joueur2:Game.player2.code});
					ajax.success(function(json, statut){
						if(json.correct){
							addMsg("Sélection des joueurs confirmée.","success");
							if(json.rouge == Game.player1.code){
								Game.player1.color = 'rouge';
								Game.player2.color = 'jaune';
								Game.currentPlayer = Game.player1;
							}
							else{
								Game.player1.color = 'jaune';
								Game.player2.color = 'rouge';
								Game.currentPlayer = Game.player2;
							}
							addMsg(Game.player1.name+" est "+Game.player1.color+", "+Game.player2.name+" est "+Game.player2.color + ".","success");
							Game.askReady();
						}
						else{
							Game.error("Erreur lors de la confirmation des joueurs.", 3);
						}
					})
				}
				else{
					Game.error("Game.playerSelection a été appelé, mais les deux joueurs sont déjà définis.", 2)
				}
			});
			$playerSelection.append($selectableElement);
		};
		addMsg("Sélectionnez le joueur 1.");
	},
	askReady : function(){
		$controlPlay = $("#control-play");
		$controlStartButton = $("#control-start");
		$controlStartButton.one("click",function(){
			$controlPlay.addClass("hide");
			$controlStartButton.addClass("hide");
			Game.playTurn();
		});
		$controlPlay.removeClass("hide");
		$controlStartButton.removeClass("hide");
	},
	askNext : function(){
		$controlPlay = $("#control-play");
		$controlNextButton = $("#control-next");
		$controlNextButton.one("click",function(){
			$controlPlay.addClass("hide");
			$controlNextButton.addClass("hide");
			Game.playTurn();
		});
		$controlPlay.removeClass("hide");
		$controlNextButton.removeClass("hide");
	},
	askColumn : function(){
		addMsg(Game.currentPlayer.name+" ("+Game.currentPlayer.color+") doit choisir une colonne.");
		var clickHandler = function(event){
			var $this = $(this);
			$(".board-column").removeClass("selectable")
				.unbind("click")
				.unbind("mouseenter")
				.unbind("mouseleave");
			var col = $this.attr("data-num-col");
			var ajax = ajaxGETjson('validHumanPlay', {"col":col});
			ajax.success(function(json, statut){
				if(json.correct){
					if(json.gameStatus === "invalid"){
						addMsg("La colonne "+col+" n'est pas un coup valide !", "danger");
						Game.playTurn();
					}
					else{
						addMsg(Game.currentPlayer.name+" ("+Game.currentPlayer.color+") joue en ["+json.colPlayed+","+json.rowPlayed+"]", Game.currentPlayer.color,function(){
							$("#cell-"+json.colPlayed+"-"+json.rowPlayed).find(".board-token").toggleClass("active");
						});
						Game.insertToken(json.colPlayed, json.rowPlayed);
						switch(json.gameStatus){
							case "continue" :
								Game.switchPlayer();
								Game.playTurn();
								break;
							case "win" :
								addMsg(Game.currentPlayer.name+" ("+Game.currentPlayer.color+") a gagné la partie !", "win");
								Game.reset();
								break;
							case "draw" :
								addMsg("Haa bah bravo ! Vous avez fait égalité... C'est malin...", "win");
								Game.reset();
								break;
							default :
								Game.error("Erreur lors de la récupération du coup de "+Game.currentPlayer.name+". gameStatus : '"+json.gameStatus+"' est inconnu.", 7);
						}
					}
				}
				else{
					Game.error("Erreur lors de la récupération du coup de "+Game.currentPlayer.name+".", 6);
				}
			});
		}
		var $previewToken = $('<div class="board-token active '+Game.currentPlayer.color+'"></div>');
		var hoverInHandler = function(){
			$(this).find(".board-row.empty-cell").last().append($previewToken);
		};
		var hoverOutHandler = function(){
			$previewToken.remove();
		};
		$(".board-column").addClass("selectable")
			.on("click",clickHandler)
			.on("mouseenter",hoverInHandler)
			.on("mouseleave",hoverOutHandler);
	},
	playTurn : function(){
		addMsg("C'est le tour de "+Game.currentPlayer.name+" ("+Game.currentPlayer.color+") !");
		// is it human or IA ?
		if(Game.currentPlayer.code == 1){
			// Human
			Game.askColumn();
		}
		else{
			// IA
			var ajax = ajaxGETjson('playFromIA', {});
			ajax.success(function(json, statut){
				if(json.correct){
					addMsg(Game.currentPlayer.name+"("+Game.currentPlayer.color+") joue en ["+json.colPlayed+","+json.rowPlayed+"]", Game.currentPlayer.color,function(){
						$("#cell-"+json.colPlayed+"-"+json.rowPlayed).find(".board-token").toggleClass("active");
					});
					Game.insertToken(json.colPlayed, json.rowPlayed);
					switch(json.gameStatus){
						case "continue" :
							Game.switchPlayer();
							if($("#control-autoplay").is(":checked")){
								Game.playTurn();
							}
							else{
								Game.askNext();
							}
							break;
						case "win" :
							addMsg(Game.currentPlayer.name+" ("+Game.currentPlayer.color+") a gagné la partie !", "win");
							Game.reset();
							break;
						case "draw" :
							addMsg("Haa bah bravo ! Vous avez fait égalité... C'est malin...", "win");
							Game.reset();
							break;
						default :
							Game.error("Erreur lors de la récupération du coup de "+Game.currentPlayer.name+". gameStatus : '"+json.gameStatus+"' est inconnu.", 5);
					}
				}
				else{
					Game.error("Erreur lors de la récupération du coup de "+Game.currentPlayer.name+".", 4);
				}
			});
		}
	},
	insertToken : function(col,row){
		var $cell = $("#cell-"+col+"-"+row);
		$cell.html('<div class="board-token '+Game.currentPlayer.color+'"></div>');
		$cell.removeClass("empty-cell");
	},
	switchPlayer : function(){
		this.currentPlayer = (this.currentPlayer === this.player1)?this.player2:this.player1;
	},
	error : function(msg, code){
		addMsg("Désolé, une erreur est survenue.","danger");
		console.log("ERROR CODE : "+code);
		console.log("ERROR MESSAGE : "+msg);
	}

};


// document.ready
$(function(){
	addMsg("jQuery est chargé.","success");
	$(".glyphicon-minus, .glyphicon-plus").click(function(){
		$this = $(this);
		if($this.hasClass("glyphicon-minus")){
			$this.removeClass("glyphicon-minus").addClass("glyphicon-plus");
		}
		else{
			$this.removeClass("glyphicon-plus").addClass("glyphicon-minus");
		}
	});
	Game.init()
});
