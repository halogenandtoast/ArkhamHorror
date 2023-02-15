module Arkham.Location.Cards.DeepBelowYourHouse where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( deepBelowYourHouse )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype DeepBelowYourHouse = DeepBelowYourHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepBelowYourHouse :: LocationCard DeepBelowYourHouse
deepBelowYourHouse =
  location DeepBelowYourHouse Cards.deepBelowYourHouse 4 (PerPlayer 1)

instance HasAbilities DeepBelowYourHouse where
  getAbilities (DeepBelowYourHouse attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage DeepBelowYourHouse where
  runMessage msg l@(DeepBelowYourHouse attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push
        (beginSkillTest
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          Nothing
          SkillAgility
          3
        )
      DeepBelowYourHouse <$> runMessage msg attrs
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source
      -> l
        <$ pushAll
             (replicate
               n
               (FindAndDrawEncounterCard iid (CardWithCardCode "01159"))
             )
    _ -> DeepBelowYourHouse <$> runMessage msg attrs
