module Arkham.Act.Cards.AlejandrosPrison (AlejandrosPrison (..), alejandrosPrison) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype AlejandrosPrison = AlejandrosPrison ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

alejandrosPrison :: ActCard AlejandrosPrison
alejandrosPrison = act (3, C) AlejandrosPrison Cards.alejandrosPrison Nothing

instance HasModifiersFor AlejandrosPrison where
  getModifiersFor (LocationTarget lid) (AlejandrosPrison a) = do
    isModified <- lid <=~> LocationWithAsset (assetIs Assets.alejandroVela)
    pure $ toModifiers a [ShroudModifier 2 | isModified]
  getModifiersFor _ _ = pure []

instance HasAbilities AlejandrosPrison where
  getAbilities (AlejandrosPrison a) =
    [ restrictedAbility
      a
      1
      (exists $ LocationWithAsset (assetIs Assets.alejandroVela) <> LocationWithoutClues)
      $ Objective
      $ forced AnyWindow
    | onSide C a
    ]

instance RunMessage AlejandrosPrison where
  runMessage msg a@(AlejandrosPrison attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide D attrs -> do
      lead <- getLeadPlayer
      deckCount <- getActDecksInPlayCount
      alejandroVela <- selectJust $ assetIs Assets.alejandroVela
      iids <- selectList $ NearestToLocation $ LocationWithAsset $ assetIs Assets.alejandroVela
      let
        takeControlMessage =
          chooseOrRunOne lead [targetLabel iid [TakeControlOfAsset iid alejandroVela] | iid <- iids]
        nextMessage =
          if deckCount <= 1
            then R1
            else RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
      pushAll [takeControlMessage, nextMessage]
      pure a
    _ -> AlejandrosPrison <$> runMessage msg attrs
