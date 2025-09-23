module Arkham.Location.Cards.WineCellarSpectral (wineCellarSpectral) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.AtDeathsDoorstep.Helpers
import Arkham.Trait (Trait (SilverTwilight))

newtype WineCellarSpectral = WineCellarSpectral LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wineCellarSpectral :: LocationCard WineCellarSpectral
wineCellarSpectral = location WineCellarSpectral Cards.wineCellarSpectral 5 (PerPlayer 2)

instance HasModifiersFor WineCellarSpectral where
  getModifiersFor (WineCellarSpectral a) = do
    modifySelect
      a
      (location_ "Victorian Halls")
      [ConnectedToWhen "Victorian Halls" (LocationWithId a.id)]

instance HasAbilities WineCellarSpectral where
  getAbilities (WineCellarSpectral a) =
    extendRevealed
      a
      [ mkAbility a 1 $ freeReaction $ SkillTestResult #after You (WhileInvestigating $ be a) #success
      , scenarioI18n $ hauntedI "wineCellarSpectral.haunted" a 2
      ]

instance RunMessage WineCellarSpectral where
  runMessage msg l@(WineCellarSpectral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      silverTwilight <- select (InPlayEnemy $ EnemyWithTrait SilverTwilight <> EnemyWithAnyDoom)
      chooseOneM iid $ scenarioI18n do
        withSkillTest \sid ->
          unscoped
            $ countVar 1
            $ labeled' "discoverAdditionalClues"
            $ skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
        labeledValidate' (notNull silverTwilight) "wineCellarSpectral.removeDoom" do
          chooseTargetM iid silverTwilight $ removeDoomFrom (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      silverTwilight <- select (InPlayEnemy $ EnemyWithTrait SilverTwilight)
      chooseOneM iid do
        scenarioI18n $ labeledValidate' (notNull silverTwilight) "wineCellarSpectral.placeDoom" do
          chooseTargetM iid silverTwilight $ placeDoomOn (attrs.ability 2) 1
        withI18n $ countVar 1 $ labeled' "takeDirectHorror" $ directHorror iid (attrs.ability 1) 1
      pure l
    _ -> WineCellarSpectral <$> liftRunMessage msg attrs
