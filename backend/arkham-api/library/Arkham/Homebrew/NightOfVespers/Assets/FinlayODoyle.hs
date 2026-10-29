module Arkham.Homebrew.NightOfVespers.Assets.FinlayODoyle (finlayODoyle) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.NightOfVespers.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Strategy

newtype FinlayODoyle = FinlayODoyle AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finlayODoyle :: AssetCard FinlayODoyle
finlayODoyle = ally FinlayODoyle Cards.finlayODoyle (3, 1)

instance HasModifiersFor FinlayODoyle where
  getModifiersFor (FinlayODoyle a) = controllerGets a [SkillModifier #combat 1]

instance HasAbilities FinlayODoyle where
  getAbilities (FinlayODoyle a) =
    [ playerLimit PerRound
        $ controlled a 1 (youExist can.manipulate.deck)
        $ freeReaction
        $ EnemyDefeated #after You ByAny AnyEnemy
    ]

instance RunMessage FinlayODoyle where
  runMessage msg a@(FinlayODoyle attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 3] #any (DrawFound iid 1)
      pure a
    _ -> FinlayODoyle <$> liftRunMessage msg attrs
