module Arkham.Types.Act.Cards.TheCarnevaleConspiracy
  ( TheCarnevaleConspiracy(..)
  , theCarnevaleConspiracy
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.AssetMatcher
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Decks
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype TheCarnevaleConspiracy = TheCarnevaleConspiracy ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

theCarnevaleConspiracy :: TheCarnevaleConspiracy
theCarnevaleConspiracy = TheCarnevaleConspiracy
  $ baseAttrs "82005" "The Carnevale Conspiracy" (Act 1 A) Nothing

ability :: ActAttrs -> Ability
ability a = mkAbility
  a
  1
  (ActionAbility Nothing
  $ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) Nothing]
  )

instance ActionRunner env => HasActions env TheCarnevaleConspiracy where
  getActions iid NonFast (TheCarnevaleConspiracy x) =
    pure [UseAbility iid (ability x)]
  getActions iid window (TheCarnevaleConspiracy x) = getActions iid window x

instance
  ( HasSet AssetId env AssetMatcher
  , HasList UnderneathCard env ActDeck
  , HasList UnderneathCard env AgendaDeck
  , ActRunner env
  )
  => RunMessage env TheCarnevaleConspiracy where
  runMessage msg a@(TheCarnevaleConspiracy attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      cnidathqua <- EncounterCard <$> genEncounterCard Enemies.cnidathqua
      a <$ pushAll [CreateEnemy cnidathqua, NextAct actId "82006"]
    After (PlaceUnderneath _ _) -> do
      cardsUnderneathAgenda <- map unUnderneathCard <$> getList AgendaDeck
      cardsUnderneathAct <- map unUnderneathCard <$> getList ActDeck
      let
        countInnocentRevelers = count ((== Assets.innocentReveler) . toCardDef)
        innocentRevelerCount = countInnocentRevelers cardsUnderneathAgenda
          + countInnocentRevelers cardsUnderneathAct
      a <$ when
        (innocentRevelerCount == 3)
        (push $ AdvanceAct actId (toSource attrs))
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      maskedCarnevaleGoers <- getSetList @AssetId
        (AssetWithTitle "Masked Carnevale-Goer")
      case maskedCarnevaleGoers of
        [] -> pure a
        xs -> a <$ pushAll
          [ chooseOne
              iid
              [ LookAtRevealed (toSource attrs) (AssetTarget x) | x <- xs ]
          ]
    _ -> TheCarnevaleConspiracy <$> runMessage msg attrs
