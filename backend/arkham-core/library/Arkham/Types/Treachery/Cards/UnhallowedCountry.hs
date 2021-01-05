module Arkham.Types.Treachery.Cards.UnhallowedCountry
  ( UnhallowedCountry(..)
  , unhallowedCountry
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype UnhallowedCountry = UnhallowedCountry Attrs
  deriving newtype (Show, ToJSON, FromJSON)

unhallowedCountry :: TreacheryId -> a -> UnhallowedCountry
unhallowedCountry uuid _ = UnhallowedCountry $ baseAttrs uuid "02088"

instance HasId (Maybe OwnerId) env AssetId => HasModifiersFor env UnhallowedCountry where
  getModifiersFor _ (InvestigatorTarget iid) (UnhallowedCountry attrs) =
    pure $ toModifiers
      attrs
      [ CannotPlay [AssetType] | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ (AssetTarget aid) (UnhallowedCountry attrs) = do
    miid <- fmap unOwnerId <$> getId aid
    case miid of
      Just iid ->
        pure $ toModifiers attrs [ Blank | treacheryOnInvestigator iid attrs ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance HasActions env UnhallowedCountry where
  getActions i window (UnhallowedCountry attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env UnhallowedCountry where
  runMessage msg t@(UnhallowedCountry attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId $ InvestigatorTarget iid)
    ChooseEndTurn iid | treacheryOnInvestigator iid attrs -> t <$ unshiftMessage
      (RevelationSkillTest iid (TreacherySource treacheryId) SkillWillpower 3)
    PassedSkillTest _ _ source _ _ | isSource attrs source ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> UnhallowedCountry <$> runMessage msg attrs
