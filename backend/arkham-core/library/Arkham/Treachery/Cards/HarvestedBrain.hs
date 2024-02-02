module Arkham.Treachery.Cards.HarvestedBrain (
  harvestedBrain,
  HarvestedBrain (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype HarvestedBrain = HarvestedBrain TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

harvestedBrain :: TreacheryCard HarvestedBrain
harvestedBrain = treachery HarvestedBrain Cards.harvestedBrain

instance HasAbilities HarvestedBrain where
  getAbilities (HarvestedBrain attrs) =
    [mkAbility attrs 1 $ ForcedAbility $ PhaseEnds #when #investigation]

instance RunMessage HarvestedBrain where
  runMessage msg t@(HarvestedBrain attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      case treacheryAttachedTarget attrs of
        Just (LocationTarget lid) -> do
          investigators <- selectList $ investigatorAt lid
          otherInvestigators <- selectList $ NotInvestigator $ investigatorAt lid
          otherInvestigatorsWithPlayers <- traverse (traverseToSnd getPlayer) otherInvestigators
          pushAll
            $ [assignDamageAndHorror investigator (toAbilitySource attrs 1) 1 1 | investigator <- investigators]
            <> [ chooseOne
                player
                [ assignDamageLabel investigator (toAbilitySource attrs 1) 1
                , assignHorrorLabel investigator (toAbilitySource attrs 1) 1
                ]
               | (investigator, player) <- otherInvestigatorsWithPlayers
               ]
        _ -> error "incorrect target"
      pure t
    _ -> HarvestedBrain <$> runMessage msg attrs
