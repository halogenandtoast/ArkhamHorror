module Arkham.Types.Treachery.Cards.UnhallowedCountry
  ( UnhallowedCountry(..)
  , unhallowedCountry
  ) where

import Arkham.Prelude

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.TreacheryId


import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype UnhallowedCountry = UnhallowedCountry TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unhallowedCountry :: TreacheryId -> a -> UnhallowedCountry
unhallowedCountry uuid _ = UnhallowedCountry $ baseAttrs uuid "02088"

instance (HasSet Trait env AssetId, HasId (Maybe OwnerId) env AssetId) => HasModifiersFor env UnhallowedCountry where
  getModifiersFor _ (InvestigatorTarget iid) (UnhallowedCountry attrs) =
    pure $ toModifiers
      attrs
      [ CannotPlay [(AssetType, singleton Ally)]
      | treacheryOnInvestigator iid attrs
      ]
  getModifiersFor _ (AssetTarget aid) (UnhallowedCountry attrs) = do
    traits <- getSet @Trait aid
    miid <- fmap unOwnerId <$> getId aid
    pure $ case miid of
      Just iid -> toModifiers
        attrs
        [ Blank | treacheryOnInvestigator iid attrs && Ally `member` traits ]
      Nothing -> []
  getModifiersFor _ _ _ = pure []

instance HasActions env UnhallowedCountry where
  getActions i window (UnhallowedCountry attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env UnhallowedCountry where
  runMessage msg t@(UnhallowedCountry attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId $ InvestigatorTarget iid)
    ChooseEndTurn iid | treacheryOnInvestigator iid attrs -> t <$ unshiftMessage
      (RevelationSkillTest iid (TreacherySource treacheryId) SkillWillpower 3)
    PassedSkillTest _ _ source _ _ _ | isSource attrs source ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> UnhallowedCountry <$> runMessage msg attrs
