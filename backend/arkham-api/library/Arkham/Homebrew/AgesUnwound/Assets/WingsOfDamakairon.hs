module Arkham.Homebrew.AgesUnwound.Assets.WingsOfDamakairon (wingsOfDamakairon) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Criteria qualified as Criteria
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorLocation))
import Arkham.Matcher
import Arkham.Projection

newtype WingsOfDamakairon = WingsOfDamakairon AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Health 2, which the def cannot carry.

TODO(ages-unwound): the card is __Fast__, which lives on the def as
@cdFastWindow@. @CardDefs/Assets.hs@ is the orchestrator's, so it still plays as
an action.
-}
wingsOfDamakairon :: AssetCard WingsOfDamakairon
wingsOfDamakairon = assetWith WingsOfDamakairon Cards.wingsOfDamakairon (healthL ?~ 2)

-- | "You get +1 [agility]."
instance HasModifiersFor WingsOfDamakairon where
  getModifiersFor (WingsOfDamakairon a) = controllerGets a [SkillModifier #agility 1]

{- | "[free] Exhaust Wings of Damakairon: Your location gets -2 shroud for this
skill test. You may not use this ability during your turn."
-}
instance HasAbilities WingsOfDamakairon where
  getAbilities (WingsOfDamakairon a) =
    [ controlled a 1 (DuringSkillTest AnySkillTest <> Criteria.Negate (Criteria.DuringTurn You))
        $ FastAbility (exhaust a)
    ]

instance RunMessage WingsOfDamakairon where
  runMessage msg a@(WingsOfDamakairon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        field InvestigatorLocation iid >>= traverse_ \lid ->
          skillTestModifier sid (attrs.ability 1) lid (ShroudModifier (-2))
      pure a
    _ -> WingsOfDamakairon <$> liftRunMessage msg attrs
