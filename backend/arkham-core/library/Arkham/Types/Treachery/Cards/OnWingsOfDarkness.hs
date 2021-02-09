module Arkham.Types.Treachery.Cards.OnWingsOfDarkness where


import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype OnWingsOfDarkness = OnWingsOfDarkness TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onWingsOfDarkness :: TreacheryId -> a -> OnWingsOfDarkness
onWingsOfDarkness uuid _ = OnWingsOfDarkness $ baseAttrs uuid "01173"

instance HasModifiersFor env OnWingsOfDarkness where
  getModifiersFor = noModifiersFor

instance HasActions env OnWingsOfDarkness where
  getActions i window (OnWingsOfDarkness attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env OnWingsOfDarkness where
  runMessage msg t@(OnWingsOfDarkness attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [RevelationSkillTest iid source SkillAgility 4, Discard (toTarget attrs)]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        centralLocations <- getSetList [Central]
        t <$ unshiftMessages
          ([ InvestigatorAssignDamage iid source DamageAny 1 1
           , UnengageNonMatching iid [Nightgaunt]
           ]
          <> [chooseOne iid [ MoveTo iid lid | lid <- centralLocations ]]
          )
    _ -> OnWingsOfDarkness <$> runMessage msg attrs
