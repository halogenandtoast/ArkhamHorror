module Arkham.Story.Cards.LightsOut (lightsOut) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Trait (Trait (Public, Restricted))

newtype LightsOut = LightsOut StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightsOut :: StoryCard LightsOut
lightsOut = persistStory $ story LightsOut Cards.lightsOut

instance HasModifiersFor LightsOut where
  getModifiersFor (LightsOut a) = do
    modifySelect a (LocationWithToken #damage) [ShroudModifier 1]
    selectEach (AbilityOnLocation (LocationWithToken #damage)) \ab -> do
      eachInvestigator \iid -> do
        modified_
          a
          (AbilityTarget iid ab.ref)
          [AdditionalCost $ OrCost [AdditionalActionCost, HorrorCost (toSource a) YouTarget 1]]

instance HasAbilities LightsOut where
  getAbilities (LightsOut a) =
    [ restricted a 1 (youExist (at_ "Security Office") <> exists (LocationWithToken #damage))
        $ actionAbilityWithCost (ClueCost (PerPlayer 1))
    ]

instance RunMessage LightsOut where
  runMessage msg s@(LightsOut attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      selectEach Anywhere $ placeTokensOn attrs #damage 1
      pure $ LightsOut $ attrs & placementL .~ Global
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lightPublic <- selectAny (LocationWithTrait Public <> LocationWithToken #damage)
      lightRestricted <- selectAny (LocationWithTrait Restricted <> LocationWithToken #damage)
      chooseOneM iid $ scenarioI18n do
        labeledValidate' lightPublic "lightOut.public" do
          selectEach (LocationWithTrait Public) $ removeAllOfTokenOn (attrs.ability 1) #damage
        labeledValidate' lightRestricted "lightOut.restricted" do
          selectEach (LocationWithTrait Restricted) $ removeAllOfTokenOn (attrs.ability 1) #damage
      pure s
    _ -> LightsOut <$> liftRunMessage msg attrs
