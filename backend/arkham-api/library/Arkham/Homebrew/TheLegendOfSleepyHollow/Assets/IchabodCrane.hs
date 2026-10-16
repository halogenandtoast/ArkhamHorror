module Arkham.Homebrew.TheLegendOfSleepyHollow.Assets.IchabodCrane (ichabodCrane) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype IchabodCrane = IchabodCrane AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichabodCrane :: AssetCard IchabodCrane
ichabodCrane = ally IchabodCrane Cards.ichabodCrane (0, 3)

instance HasModifiersFor IchabodCrane where
  getModifiersFor (IchabodCrane a) = for_ a.controller \iid ->
    modified_ a iid [SkillModifier #willpower 1, SkillModifier #intellect 1]

instance HasAbilities IchabodCrane where
  getAbilities (IchabodCrane a) =
    [ scenarioI18n
        $ withI18nTooltip "ichabodCrane.vigil"
        $ controlled a 1 NoRestriction
        $ triggered (PlacedDoomCounter #after AnySource (TargetAtLocation YourLocation)) (exhaust a)
    ]

instance RunMessage IchabodCrane where
  runMessage msg a@(IchabodCrane attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid
      chooseOneM iid $ scenarioI18n $ scope "ichabodCrane" do
        labeled "drawCard" $ drawCards iid (attrs.ability 1) 1
        when (notNull assets) do
          labeled "placeCharge" do
            chooseTargetM iid assets \aid -> placeTokens (attrs.ability 1) aid #charge 1
      pure a
    _ -> IchabodCrane <$> liftRunMessage msg attrs
