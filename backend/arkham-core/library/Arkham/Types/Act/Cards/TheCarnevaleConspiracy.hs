module Arkham.Types.Act.Cards.TheCarnevaleConspiracy
  ( TheCarnevaleConspiracy(..)
  , theCarnevaleConspiracy
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Decks
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype TheCarnevaleConspiracy = TheCarnevaleConspiracy ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCarnevaleConspiracy :: ActCard TheCarnevaleConspiracy
theCarnevaleConspiracy =
  act (1, A) TheCarnevaleConspiracy Cards.theCarnevaleConspiracy Nothing

ability :: ActAttrs -> Ability
ability a = mkAbility
  a
  1
  (ActionAbility Nothing
  $ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) Anywhere]
  )

instance ActionRunner env => HasAbilities env TheCarnevaleConspiracy where
  getAbilities _ (Window Timing.When NonFast) (TheCarnevaleConspiracy x) = do
    maskedCarnevaleGoers <- selectList (AssetWithTitle "Masked Carnevale-Goer")
    filteredMaskedCarnevaleGoers <- flip filterM maskedCarnevaleGoers $ \aid ->
      do
        modifiers' <- getModifiers (toSource x) (AssetTarget aid)
        pure $ CannotBeRevealed `notElem` modifiers'
    pure [ ability x | notNull filteredMaskedCarnevaleGoers ]
  getAbilities iid window (TheCarnevaleConspiracy x) =
    getAbilities iid window x

instance
  ( HasList UnderneathCard env ActDeck
  , HasList UnderneathCard env AgendaDeck
  , HasModifiersFor env ()
  , ActRunner env
  )
  => RunMessage env TheCarnevaleConspiracy where
  runMessage msg a@(TheCarnevaleConspiracy attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      cnidathqua <- EncounterCard <$> genEncounterCard Enemies.cnidathqua
      maskedCarnevaleGoers <- selectList
        (AssetWithTitle "Masked Carnevale-Goer")
      let
        flipMsg = case maskedCarnevaleGoers of
          [] -> []
          xs ->
            [ chooseOne
                leadInvestigatorId
                [ Flip (toSource attrs) (AssetTarget x) | x <- xs ]
            ]
      a <$ pushAll ([CreateEnemy cnidathqua, NextAct actId "82006"] <> flipMsg)
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
      maskedCarnevaleGoers <- selectList
        (AssetWithTitle "Masked Carnevale-Goer")
      filteredMaskedCarnevaleGoers <-
        flip filterM maskedCarnevaleGoers $ \aid -> do
          modifiers' <- getModifiers source (AssetTarget aid)
          pure $ CannotBeRevealed `notElem` modifiers'
      case filteredMaskedCarnevaleGoers of
        [] -> pure a
        xs -> a <$ pushAll
          [ chooseOne
              iid
              [ LookAtRevealed (toSource attrs) (AssetTarget x) | x <- xs ]
          ]
    _ -> TheCarnevaleConspiracy <$> runMessage msg attrs
