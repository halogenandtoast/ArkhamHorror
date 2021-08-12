module Arkham.Types.Treachery.Cards.DraggedUnder where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype DraggedUnder = DraggedUnder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

draggedUnder :: TreacheryCard DraggedUnder
draggedUnder = treachery DraggedUnder Cards.draggedUnder

instance (TreacheryRunner env) => RunMessage env DraggedUnder where
  runMessage msg t@(DraggedUnder attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillAgility 3)
    MoveFrom iid _ | treacheryOnInvestigator iid attrs -> t <$ pushAll
      [ InvestigatorAssignDamage iid (TreacherySource treacheryId) DamageAny 2 0
      , Discard (TreacheryTarget treacheryId)
      ]
    EndTurn iid | treacheryOnInvestigator iid attrs -> t <$ push
      (BeginSkillTest
        iid
        (TreacherySource treacheryId)
        (InvestigatorTarget iid)
        Nothing
        SkillAgility
        3
      )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ when
        (isNothing treacheryAttachedTarget)
        (push $ AttachTreachery treacheryId (InvestigatorTarget iid))
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ push (Discard $ toTarget attrs)
    _ -> DraggedUnder <$> runMessage msg attrs
