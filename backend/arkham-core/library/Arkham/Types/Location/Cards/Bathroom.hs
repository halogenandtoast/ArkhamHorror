module Arkham.Types.Location.Cards.Bathroom where


import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype Bathroom = Bathroom LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bathroom :: Bathroom
bathroom = Bathroom $ baseAttrs
  "50016"
  (Name "Bathroom" Nothing)
  EncounterSet.CurseOfTheRougarou
  1
  (PerPlayer 1)
  Star
  [T]
  mempty

instance HasModifiersFor env Bathroom where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Bathroom where
  getActions i window (Bathroom attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Bathroom where
  runMessage msg l@(Bathroom attrs) = case msg of
    After (RevealToken (SkillTestSource _ _ source (Just Action.Investigate)) iid tokenFace)
      | isSource attrs source
      -> l <$ when
        (tokenFace `elem` [Skull, Cultist, Tablet, AutoFail])
        (unshiftMessages [SetActions iid (toSource attrs) 0, ChooseEndTurn iid])
    _ -> Bathroom <$> runMessage msg attrs
