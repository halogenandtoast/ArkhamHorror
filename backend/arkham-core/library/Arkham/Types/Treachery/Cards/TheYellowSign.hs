{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.TheYellowSign where

import Arkham.Import

import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype TheYellowSign = TheYellowSign Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theYellowSign :: TreacheryId -> a -> TheYellowSign
theYellowSign uuid _ = TheYellowSign $ baseAttrs uuid "01176"

instance HasModifiersFor env TheYellowSign where
  getModifiersFor = noModifiersFor

instance HasActions env TheYellowSign where
  getActions i window (TheYellowSign attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env TheYellowSign where
  runMessage msg t@(TheYellowSign attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ unshiftMessages
        [ BeginSkillTest
          iid
          source
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          4
        , Discard $ toTarget attrs
        ]
    FailedSkillTest iid _ source _ _ | isSource attrs source ->
      t <$ unshiftMessages
        [ InvestigatorAssignDamage iid (TreacherySource treacheryId) 0 2
        , SearchDeckForTraits iid (InvestigatorTarget iid) [Madness] -- TODO: We may need to specify weakness
        ]
    _ -> TheYellowSign <$> runMessage msg attrs
