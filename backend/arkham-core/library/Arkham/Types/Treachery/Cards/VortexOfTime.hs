module Arkham.Types.Treachery.Cards.VortexOfTime
  ( vortexOfTime
  , VortexOfTime(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype VortexOfTime = VortexOfTime TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vortexOfTime :: TreacheryCard VortexOfTime
vortexOfTime = treachery VortexOfTime Cards.vortexOfTime

instance HasModifiersFor env VortexOfTime where
  getModifiersFor = noModifiersFor

instance HasActions env VortexOfTime where
  getActions i window (VortexOfTime attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env VortexOfTime where
  runMessage msg t@(VortexOfTime attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      sentinelHills <- getSetList @LocationId [SentinelHill]
      investigatorsAtSentinelHills <- concatMapM' getSetList sentinelHills
      t <$ unshiftMessages
        ([ BeginSkillTest
             iid
             source
             (InvestigatorTarget iid)
             Nothing
             SkillWillpower
             4
         | iid <- investigatorsAtSentinelHills
         ]
        <> [discard attrs]
        )
    FailedSkillTest iid _ source SkillTestTarget{} _ _
      | isSource attrs source
      -> t <$ unshiftMessage (InvestigatorAssignDamage iid source DamageAny 2 0)
    _ -> VortexOfTime <$> runMessage msg attrs
