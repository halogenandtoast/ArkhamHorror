{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.DreamsOfRlyeh where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.FastWindow
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype DreamsOfRlyeh = DreamsOfRlyeh Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dreamsOfRlyeh :: TreacheryId -> DreamsOfRlyeh
dreamsOfRlyeh uuid = DreamsOfRlyeh $ baseAttrs uuid "01182"

instance (IsInvestigator investigator) => HasActions env investigator DreamsOfRlyeh where
  getActions i NonFast (DreamsOfRlyeh Attrs {..}) = pure
    [ ActivateCardAbilityAction
        (getId () i)
        (mkAbility (TreacherySource treacheryId) 1 (ActionAbility 1 Nothing))
    | treacheryAttachedInvestigator == Just (getId () i)
    ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env DreamsOfRlyeh where
  runMessage msg t@(DreamsOfRlyeh attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessages
        [ AttachTreacheryToInvestigator tid iid
        , AddModifier
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower (-1) (TreacherySource tid))
        , AddModifier
          (InvestigatorTarget iid)
          (SanityModifier (-1) (TreacherySource tid))
        ]
      DreamsOfRlyeh <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    UseCardAbility iid _ (TreacherySource tid) 1 | tid == treacheryId ->
      t <$ unshiftMessage
        (BeginSkillTest
          iid
          (TreacherySource treacheryId)
          Nothing
          SkillWillpower
          3
          [Discard (TreacheryTarget treacheryId)]
          mempty
          mempty
          mempty
        )
    _ -> DreamsOfRlyeh <$> runMessage msg attrs
