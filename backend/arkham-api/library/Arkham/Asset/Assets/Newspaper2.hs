module Arkham.Asset.Assets.Newspaper2 (newspaper2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Projection

newtype Newspaper2 = Newspaper2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newspaper2 :: AssetCard Newspaper2
newspaper2 = asset Newspaper2 Cards.newspaper2

instance HasModifiersFor Newspaper2 where
  getModifiersFor (Newspaper2 a) = for_ a.controller \iid -> do
    clueCount <- field InvestigatorClues iid
    modifiedWhen_ a (clueCount == 0) iid [ActionSkillModifier #investigate #intellect 2]

instance HasAbilities Newspaper2 where
  getAbilities (Newspaper2 a) =
    [ controlled a 1 (youExist $ InvestigatorWithoutAnyClues <> at_ LocationWithAnyClues)
        $ freeReaction
        $ Matcher.WouldDiscoverClues #when You YourLocation (atLeast 1)
    ]

instance RunMessage Newspaper2 where
  runMessage msg a@(Newspaper2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      pure a
    _ -> Newspaper2 <$> liftRunMessage msg attrs
