module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.ParisSewers_022 (parisSewers_022) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ParisSewers_022 = ParisSewers_022 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parisSewers_022 :: LocationCard ParisSewers_022
parisSewers_022 =
  locationWith ParisSewers_022 Cards.parisSewers_022 5 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities ParisSewers_022 where
  getAbilities (ParisSewers_022 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "parisSewers_022.brace"
      $ limitedAbility (GroupLimit PerTest 2)
      $ restricted a 1 (Here <> DuringSkillTest (SkillTestOfInvestigator (investigatorAt a.id)))
      $ FastAbility (ExhaustAssetCost (AssetControlledBy You <> AssetReady <> not_ PermanentAsset))

instance RunMessage ParisSewers_022 where
  runMessage msg l@(ParisSewers_022 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid (Difficulty (-1))
      pure l
    _ -> ParisSewers_022 <$> liftRunMessage msg attrs
