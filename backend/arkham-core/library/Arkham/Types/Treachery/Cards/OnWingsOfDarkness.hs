module Arkham.Types.Treachery.Cards.OnWingsOfDarkness where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype OnWingsOfDarkness = OnWingsOfDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onWingsOfDarkness :: TreacheryCard OnWingsOfDarkness
onWingsOfDarkness = treachery OnWingsOfDarkness Cards.onWingsOfDarkness

instance TreacheryRunner env => RunMessage env OnWingsOfDarkness where
  runMessage msg t@(OnWingsOfDarkness attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillAgility 4)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        centralLocations <- getSetList [Central]
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
