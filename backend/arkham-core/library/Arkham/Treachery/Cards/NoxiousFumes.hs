module Arkham.Treachery.Cards.NoxiousFumes (
  noxiousFumes,
  NoxiousFumes (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement
import Arkham.SkillTest.Type
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype NoxiousFumes = NoxiousFumes TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

noxiousFumes :: TreacheryCard NoxiousFumes
noxiousFumes = treachery NoxiousFumes Cards.noxiousFumes

instance RunMessage NoxiousFumes where
  runMessage msg t@(NoxiousFumes attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- selectList $ colocatedWith iid
      investigatorPlayers <- traverse (traverseToSnd getPlayer) investigators

      pushAll
        [ chooseOne
          player
          [ SkillLabel skill [beginSkillTest investigator (toSource attrs) investigator skill 3]
          | skill <- [#agility, #combat]
          ]
        | (investigator, player) <- investigatorPlayers
        ]

      pure t
    PassedSkillTest iid _ (isSource attrs -> True) Initiator {} (SkillSkillTest SkillAgility) _ -> do
      accessibleLocationIds <- accessibleLocations iid
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ TargetLabel (LocationTarget lid) [Move $ move attrs iid lid]
          | lid <- accessibleLocationIds
          ]
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) Initiator {} (SkillSkillTest SkillAgility) _ -> do
      push $ assignDamage iid (toSource attrs) 2
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) Initiator {} (SkillSkillTest SkillCombat) n -> do
      push $ assignDamage iid (toSource attrs) n
      pure t
    _ -> NoxiousFumes <$> runMessage msg attrs
