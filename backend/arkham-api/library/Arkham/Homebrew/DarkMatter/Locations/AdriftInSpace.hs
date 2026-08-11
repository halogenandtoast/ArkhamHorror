module Arkham.Homebrew.DarkMatter.Locations.AdriftInSpace (adriftInSpace) where

import Arkham.Ability
import Arkham.ChaosToken.Types qualified as CT
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype AdriftInSpace = AdriftInSpace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adriftInSpace :: LocationCard AdriftInSpace
adriftInSpace = location AdriftInSpace Cards.adriftInSpace 2 (PerPlayer 1)

{- | "Forced - After you reveal a +1, 0 or [elder_sign] token while investigating
this location: Reveal and resolve an additional chaos token for this skill test."
-}
instance HasAbilities AdriftInSpace where
  getAbilities (AdriftInSpace a) =
    extendRevealed1 a
      $ restricted a 1 (DuringSkillTest $ YourSkillTest $ WhileInvestigating $ be a)
      $ forced
      $ RevealChaosToken #after You
      $ oneOf [ChaosTokenFaceIs CT.PlusOne, ChaosTokenFaceIs CT.Zero, ChaosTokenFaceIs CT.ElderSign]

instance RunMessage AdriftInSpace where
  runMessage msg l@(AdriftInSpace attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid RevealAnotherChaosToken
      pure l
    _ -> AdriftInSpace <$> liftRunMessage msg attrs
