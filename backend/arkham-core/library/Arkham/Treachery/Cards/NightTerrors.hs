module Arkham.Treachery.Cards.NightTerrors (nightTerrors, NightTerrors (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype NightTerrors = NightTerrors TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightTerrors :: TreacheryCard NightTerrors
nightTerrors = treachery NightTerrors Cards.nightTerrors

instance HasAbilities NightTerrors where
  getAbilities (NightTerrors a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ SkillTestResult #after You AnySkillTest
        $ FailureResult AnyValue
    , restrictedAbility a 2 (InThreatAreaOf You) actionAbility
    ]

instance RunMessage NightTerrors where
  runMessage msg t@(NightTerrors attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push
        $ lookAt
          iid
          (attrs.ability 1)
          iid
          [(FromTopOfDeck 3, RemoveRestFromGame)]
          WeaknessCard
          (DrawAllFound iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      pushAll
        [beginSkillTest iid (attrs.ability 2) iid #willpower 4, toDiscardBy iid (attrs.ability 2) attrs]
      pure t
    _ -> NightTerrors <$> runMessage msg attrs
