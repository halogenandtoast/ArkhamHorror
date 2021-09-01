module Arkham.Types.Location.Cards.DeepBelowYourHouse where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (deepBelowYourHouse)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype DeepBelowYourHouse = DeepBelowYourHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepBelowYourHouse :: LocationCard DeepBelowYourHouse
deepBelowYourHouse = location
  DeepBelowYourHouse
  Cards.deepBelowYourHouse
  4
  (PerPlayer 1)
  Squiggle
  [Plus]

instance HasAbilities env DeepBelowYourHouse where
  getAbilities i window (DeepBelowYourHouse attrs) =
    withBaseAbilities i window attrs $ pure
      [ mkAbility attrs 1
        $ ForcedAbility
        $ RevealLocation Timing.After You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env DeepBelowYourHouse where
  runMessage msg l@(DeepBelowYourHouse attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push
        (BeginSkillTest
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
