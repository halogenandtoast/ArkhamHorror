module Arkham.Types.Treachery.Cards.VastExpanse
  ( vastExpanse
  , VastExpanse(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId

newtype VastExpanse = VastExpanse TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vastExpanse :: TreacheryId -> a -> VastExpanse
vastExpanse uuid _ = VastExpanse $ baseAttrs uuid "02333"

instance HasModifiersFor env VastExpanse where
  getModifiersFor = noModifiersFor

instance HasActions env VastExpanse where
  getActions i window (VastExpanse attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env VastExpanse where
  runMessage msg t@(VastExpanse attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      extradimensionalCount <- length
        <$> getSetList @LocationId [Extradimensional]
      let
        revelationMsg = if extradimensionalCount == 0
          then Surge iid source
          else BeginSkillTest
            iid
            source
            (InvestigatorTarget iid)
            Nothing
            SkillWillpower
            (min 5 extradimensionalCount)
      t <$ unshiftMessages [revelationMsg, Discard (toTarget attrs)]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source
      -> t <$ unshiftMessage (InvestigatorAssignDamage iid source DamageAny 0 n)
    _ -> VastExpanse <$> runMessage msg attrs
