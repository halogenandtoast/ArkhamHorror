module Arkham.Asset.Cards.TrenchKnife (trenchKnife, TrenchKnife (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype TrenchKnife = TrenchKnife AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trenchKnife :: AssetCard TrenchKnife
trenchKnife = asset TrenchKnife Cards.trenchKnife

instance HasModifiersFor TrenchKnife where
  getModifiersFor (InvestigatorTarget iid) (TrenchKnife attrs) | attrs `controlledBy` iid = do
    pure $ toModifiers attrs [ActionDoesNotCauseAttacksOfOpportunity #engage]
  getModifiersFor _ _ = pure []

instance HasAbilities TrenchKnife where
  getAbilities (TrenchKnife attrs) = [restrictedAbility attrs 1 ControlsThis fightAction_]

instance RunMessage TrenchKnife where
  runMessage msg a@(TrenchKnife attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemyCount <- selectCount $ enemyEngagedWith iid
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll
        [ skillTestModifier sid attrs iid (SkillModifier #combat enemyCount)
        , chooseFight
        ]
      pure a
    _ -> TrenchKnife <$> runMessage msg attrs
