module Arkham.Homebrew.DarkMatter.Assets.ParticleAccelerator (particleAccelerator) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype ParticleAccelerator = ParticleAccelerator AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

particleAccelerator :: AssetCard ParticleAccelerator
particleAccelerator = asset ParticleAccelerator Cards.particleAccelerator

{- | "[reaction] After you reveal a [tablet], [elderthing], or [autofail] during
a skill test, spend 1 resource and exhaust Particle Accelerator: Gain 1 clue
from the token bank."
-}
instance HasAbilities ParticleAccelerator where
  getAbilities (ParticleAccelerator a) =
    [ controlled_ a 1
        $ triggered
          ( RevealChaosTokensDuringSkillTest #after You (YourSkillTest AnySkillTest)
              $ oneOf [#tablet, #elderthing, #autofail]
          )
          (exhaust a <> ResourceCost 1)
    ]

instance RunMessage ParticleAccelerator where
  runMessage msg a@(ParticleAccelerator attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainClues iid (attrs.ability 1) 1
      pure a
    _ -> ParticleAccelerator <$> liftRunMessage msg attrs
