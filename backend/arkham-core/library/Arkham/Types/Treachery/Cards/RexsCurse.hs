{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.RexsCurse where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype RexsCurse = RexsCurse Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

rexsCurse :: TreacheryId -> Maybe InvestigatorId -> RexsCurse
rexsCurse uuid iid = RexsCurse $ weaknessAttrs uuid iid "02009"

instance HasModifiersFor env RexsCurse where
  getModifiersFor = noModifiersFor

instance HasActions env RexsCurse where
  getActions _ _ _ = pure []

instance TreacheryRunner env => RunMessage env RexsCurse where
  runMessage msg t@(RexsCurse attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessage (AttachTreachery treacheryId (InvestigatorTarget iid))
      RexsCurse <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    Will (PassedSkillTest iid _ _ SkillTestInitiatorTarget{} _)
      | Just iid == treacheryAttachedInvestigator -> do
        let
          ability = (mkAbility (TreacherySource treacheryId) 0 ForcedAbility)
            { abilityLimit = PerTestOrAbility
            }
        usedAbilities <- map unUsedAbility <$> getList ()
        when ((iid, ability) `notElem` usedAbilities) $ do
          retainedMessages <- withQueue $ \queue ->
            let
              (remainingWillPass, queue') = flip span queue $ \case
                Will PassedSkillTest{} -> True
                _ -> False
              (before, after) = flip break queue' $ \case
                Ask iid' (ChooseOne [SkillTestApplyResults]) | iid == iid' ->
                  True
                _ -> False
              remaining = case after of
                [] -> []
                (_ : xs) -> xs
            in (before <> remaining, remainingWillPass)
          unshiftMessages
            $ retainedMessages
            <> [ ActivateCardAbilityAction iid ability
               , ReturnSkillTestRevealedTokens
               , AddSkillTestSubscriber (TreacheryTarget treacheryId)
               , DrawAnotherToken iid
               ]
        pure t
    FailedSkillTest iid _ _ (TreacheryTarget tid) _ | tid == treacheryId -> do
      t <$ unshiftMessage (ShuffleIntoDeck iid (TreacheryTarget treacheryId))
    _ -> RexsCurse <$> runMessage msg attrs
