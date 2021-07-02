module Arkham.Types.Treachery.Cards.DreamsOfRlyeh
  ( DreamsOfRlyeh(..)
  , dreamsOfRlyeh
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner
import Arkham.Types.Window

newtype DreamsOfRlyeh = DreamsOfRlyeh TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamsOfRlyeh :: TreacheryCard DreamsOfRlyeh
dreamsOfRlyeh = treachery DreamsOfRlyeh Cards.dreamsOfRlyeh

instance HasModifiersFor env DreamsOfRlyeh where
  getModifiersFor _ (InvestigatorTarget iid) (DreamsOfRlyeh attrs) =
    pure $ toModifiers attrs $ if treacheryOnInvestigator iid attrs
      then [SkillModifier SkillWillpower (-1), SanityModifier (-1)]
      else []
  getModifiersFor _ _ _ = pure []

instance HasActions env DreamsOfRlyeh where
  getActions iid NonFast (DreamsOfRlyeh a) = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 1))
    | treacheryOnInvestigator iid a
    ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env DreamsOfRlyeh where
  runMessage msg t@(DreamsOfRlyeh attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId (InvestigatorTarget iid))
    UseCardAbility iid (TreacherySource tid) _ 1 _ | tid == treacheryId ->
      t <$ unshiftMessage
        (BeginSkillTest
          iid
          (TreacherySource treacheryId)
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          3
        )
    PassedSkillTest _ _ source _ _ _ | isSource attrs source ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> DreamsOfRlyeh <$> runMessage msg attrs
