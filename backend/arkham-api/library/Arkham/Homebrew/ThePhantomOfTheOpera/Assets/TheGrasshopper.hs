module Arkham.Homebrew.ThePhantomOfTheOpera.Assets.TheGrasshopper (theGrasshopper) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype TheGrasshopper = TheGrasshopper AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGrasshopper :: AssetCard TheGrasshopper
theGrasshopper = asset TheGrasshopper Cards.theGrasshopper

instance HasAbilities TheGrasshopper where
  getAbilities (TheGrasshopper a) =
    [ controlled a 1 (DuringSkillTest $ YourSkillTest #any)
        $ triggered
          (RevealChaosToken #when You (oneOf [#skull, #cultist, #tablet, #elderthing]))
          (OrCost [DamageCost (a.ability 1) YouTarget 1, HorrorCost (a.ability 1) YouTarget 1])
    , controlled a 2 ControlsThis
        $ triggered
          ( oneOf
              [ PlacedCounterOnInvestigator #after You AnySource DamageCounter (atLeast 1)
              , PlacedCounterOnInvestigator #after You AnySource HorrorCounter (atLeast 1)
              ]
          )
          (exhaust a)
    ]

instance RunMessage TheGrasshopper where
  runMessage msg a@(TheGrasshopper attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) token IgnoreChaosTokenSymbolEffects
        cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseDamageEnemy iid (attrs.ability 2) (locationWithInvestigator iid) AnyEnemy 1
      pure a
    _ -> TheGrasshopper <$> liftRunMessage msg attrs
