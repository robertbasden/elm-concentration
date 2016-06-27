import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Random
import Task
import Time

import Task.Extra

main =
    Html.program {
        init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
    }

-- MODEL

type alias Model = {
    waitingForAnimation: Bool
    , cards: List (Id, Card)
}

type alias Id = Int

type alias Card = {
    flipped: Bool
    , matched: Bool
    , cardType: CardType
}



allTypesMatch : List (CardType) -> Bool
allTypesMatch list =
    let
        uniqueCardTypes = List.foldr (\item list -> if (List.member item list) then list else (item :: list)) [] list
    in
        (List.length uniqueCardTypes) == 1

generateCards : List (Id, Card)
generateCards =
    List.map (\count -> (count, Card False False Diamond)) [1..4] ++
    List.map (\count -> (count + 4, Card False False Heart)) [1..4] ++
    List.map (\count -> (count + 8, Card False False Spade)) [1..4] ++
    List.map (\count -> (count + 12, Card False False Club)) [1..4]

shuffleCards : List (Id, Card) -> List (Id, Card)
shuffleCards cards =
    let
        generator = Random.int 0 1000
        listGenerator = Random.list (List.length cards) (Random.int 0 100)
        (listofRandomNumbers, nextSeed) = Random.step listGenerator (Random.initialSeed 31415)
        taggedCards = List.map2 (,) listofRandomNumbers cards
        sortedTaggedCards = List.sortBy fst taggedCards
        reorderedCards = List.map (\(_, cardPair) -> cardPair) sortedTaggedCards
    in
        reorderedCards

init : (Model, Cmd Msg)
init =
    (Model False (shuffleCards generateCards), Cmd.none)

-- UPDATE

type CardType = Diamond
    | Heart
    | Spade
    | Club

type Msg = NewGame
    | FlipCard Id
    | SetFlippedToMatched
    | ResetFlipped

delayedTask task =
    Task.succeed Nothing
    |> Task.Extra.delay 1000
    |> Task.Extra.performFailproof (\_ -> task)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        NewGame ->
            let
                shuffledCards = shuffleCards model.cards
            in
                ({ model | cards = (List.map (\(id, card) -> (id, { card | flipped = False, matched = False })) shuffledCards), waitingForAnimation = False }, Cmd.none)
        SetFlippedToMatched ->
            ({ model | cards = (List.map (\(id, card) -> (id, { card | matched = card.flipped })) model.cards), waitingForAnimation = False }, Cmd.none)
        ResetFlipped ->
            ({ model | cards = (List.map (\(id, card) -> (id, { card | flipped = card.matched })) model.cards), waitingForAnimation = False }, Cmd.none)
        FlipCard cardIdToFlip ->
            if model.waitingForAnimation then
                (model, Cmd.none)
            else
                let
                    newModel = { model | cards = (List.map (\(id, card) -> (id, { card | flipped = card.flipped || id == cardIdToFlip })) model.cards) }
                    flippedCards = newModel.cards
                        |> List.filter (\(id, card) -> card.flipped && card.matched == False)
                    flippedCardCount = flippedCards
                        |> List.length
                    flippedCardTypes = flippedCards
                        |> List.map (\(id, card) -> card.cardType)
                    allFlippedCardsMatch = allTypesMatch flippedCardTypes
                in
                    if flippedCardCount == 2 then
                        if allFlippedCardsMatch then
                            ({ newModel | waitingForAnimation = True }, delayedTask SetFlippedToMatched)
                        else
                            ({ newModel | waitingForAnimation = True }, delayedTask ResetFlipped)
                    else
                        (newModel, Cmd.none)

-- VIEW

cardView : Id -> Card -> Html Msg
cardView id card =
    let
        cardTypeClass = case card.cardType of
            Diamond -> "card--diamond"
            Heart -> "card--heart"
            Spade -> "card--spade"
            Club -> "card--club"
    in
        div [onClick (FlipCard id), classList [("card", True), ("card--flipped", card.flipped), ("card--matched", card.matched), (cardTypeClass, True)]] [
            div [class "card__face card__face--back"] []
            , div [class "card__face card__face--front"] []
        ]

view : Model -> Html Msg
view model =
    let
        matchedCardCount = model.cards
            |> List.filter (\(_, card) -> card.matched)
            |> List.length
        totalCardCount = (List.length model.cards)
        countDisplay = (toString matchedCardCount) ++ "/" ++ (toString totalCardCount)
    in
        div [] [
            div [class "header"] [
                button [class "btn", onClick NewGame] [text "New Game"]
                , span [class "controls"] [
                    text "Matched: "
                    , span [class "controls__count"] [
                        span [class "controls__matched-count"] [text (toString matchedCardCount)]
                        , text "/"
                        , span [class "controls__total-count"] [text (toString totalCardCount)]
                    ]
                ]
            ]
            , div [class "main"] [
                div [] (List.map (\(id, card) -> cardView id card) model.cards)
            ]
        ]

