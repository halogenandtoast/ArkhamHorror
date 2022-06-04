module Arkham.Treachery.Cards.OnWingsOfDarkness where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Attrs

newtype OnWingsOfDarkness = OnWingsOfDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor m, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onWingsOfDarkness :: TreacheryCard OnWingsOfDarkness
onWingsOfDarkness = treachery OnWingsOfDarkness Cards.onWingsOfDarkness

instance RunMessage OnWingsOfDarkness where
  runMessage msg t@(OnWingsOfDarkness attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillAgility 4)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        centralLocations <- selectList $ LocationWithTrait Central
        t <$ pushAll
          ([ InvestigatorAssignDamage iid source DamageAny 1 1
           , UnengageNonMatching iid [Nightgaunt]
           ]
          <> [ chooseOne
                 iid
                 [ TargetLabel
                     (LocationTarget lid)
                     [MoveTo (toSource attrs) iid lid]
                 | lid <- centralLocations
                 ]
             ]
          )
    _ -> OnWingsOfDarkness <$> runMessage msg attrs
