module Arkham.Asset.Cards.MartyrsVambraceRemnantOfTheUnknown3 (
  martyrsVambraceRemnantOfTheUnknown3,
  MartyrsVambraceRemnantOfTheUnknown3 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Game.Helpers (skillTestMatches)
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.EncounterCard
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher

newtype MartyrsVambraceRemnantOfTheUnknown3 = MartyrsVambraceRemnantOfTheUnknown3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

martyrsVambraceRemnantOfTheUnknown3 :: AssetCard MartyrsVambraceRemnantOfTheUnknown3
martyrsVambraceRemnantOfTheUnknown3 =
  assetWith MartyrsVambraceRemnantOfTheUnknown3 Cards.martyrsVambraceRemnantOfTheUnknown3
    $ (healthL ?~ 2)
    . (sanityL ?~ 2)

instance HasModifiersFor MartyrsVambraceRemnantOfTheUnknown3 where
  getModifiersFor (InvestigatorTarget iid) (MartyrsVambraceRemnantOfTheUnknown3 a) = do
    maybeModified a do
      guard $ a `controlledBy` iid
      st <- MaybeT getSkillTest
      liftGuardM
        $ skillTestMatches iid (toSource a) st (SkillTestFromRevelation <> SkillTestOnEncounterCard)
      pure [AnySkillValue 1]
  getModifiersFor _ _ = pure []

instance HasAbilities MartyrsVambraceRemnantOfTheUnknown3 where
  getAbilities (MartyrsVambraceRemnantOfTheUnknown3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (DrawCard #after (affectsOthers NotYou) (basic $ NonPeril <> IsEncounterCard) AnyDeck)
          (exhaust a)
    ]

instance RunMessage MartyrsVambraceRemnantOfTheUnknown3 where
  runMessage msg a@(MartyrsVambraceRemnantOfTheUnknown3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      lift $ changeEncounterCardDrawer card.id iid
      pure a
    _ -> MartyrsVambraceRemnantOfTheUnknown3 <$> liftRunMessage msg attrs
