module Arkham.Act.Cards.TheCarnevaleConspiracy
  ( TheCarnevaleConspiracy(..)
  , theCarnevaleConspiracy
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype TheCarnevaleConspiracy = TheCarnevaleConspiracy ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCarnevaleConspiracy :: ActCard TheCarnevaleConspiracy
theCarnevaleConspiracy =
  act (1, A) TheCarnevaleConspiracy Cards.theCarnevaleConspiracy Nothing

instance HasAbilities TheCarnevaleConspiracy where
  getAbilities (TheCarnevaleConspiracy x) =
    [ restrictedAbility
      x
      1
      (AssetExists
      $ AssetWithTitle "Masked Carnevale-Goer"
      <> AssetWithoutModifier CannotBeRevealed
      )
      (ActionAbility Nothing
      $ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) Anywhere]
      )
    , restrictedAbility
        x
        2
        (UnderneathCardCount
          (EqualTo $ Static 3)
          (UnderActDeck <> UnderAgendaDeck)
          (cardIs Assets.innocentReveler)
        )
      $ ForcedAbility AnyWindow
    ]

instance RunMessage TheCarnevaleConspiracy where
  runMessage msg a@(TheCarnevaleConspiracy attrs@ActAttrs {..}) = case msg of
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
              [ LookAtRevealed iid (toSource attrs) (AssetTarget x) | x <- xs ]
          ]
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      cnidathqua <- getSetAsideCard Enemies.cnidathqua
      maskedCarnevaleGoers <- selectList
        (AssetWithTitle "Masked Carnevale-Goer")
      let
        flipMsg = case maskedCarnevaleGoers of
          [] -> []
          xs ->
            [ chooseOne
                leadInvestigatorId
                [ Flip leadInvestigatorId (toSource attrs) (AssetTarget x) | x <- xs ]
            ]
      a <$ pushAll
        ([CreateEnemy cnidathqua, AdvanceActDeck actDeckId (toSource attrs)]
        <> flipMsg
        )
    _ -> TheCarnevaleConspiracy <$> runMessage msg attrs
