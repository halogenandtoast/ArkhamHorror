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
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype TheCarnevaleConspiracy = TheCarnevaleConspiracy ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
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

instance
  ( HasModifiersFor env ()
  , ActRunner env
  )
  => RunMessage env TheCarnevaleConspiracy where
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
              [ LookAtRevealed (toSource attrs) (AssetTarget x) | x <- xs ]
          ]
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
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
                [ Flip (toSource attrs) (AssetTarget x) | x <- xs ]
            ]
      a <$ pushAll ([CreateEnemy cnidathqua, NextAct actId "82006"] <> flipMsg)
    _ -> TheCarnevaleConspiracy <$> runMessage msg attrs
