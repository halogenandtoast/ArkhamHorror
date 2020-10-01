{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.RexsCurse where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype RexsCurse = RexsCurse Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

rexsCurse :: TreacheryId -> Maybe InvestigatorId -> RexsCurse
rexsCurse uuid iid = RexsCurse $ weaknessAttrs uuid iid "02009"

instance HasActions env investigator RexsCurse where
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env RexsCurse where
  runMessage msg t@(RexsCurse attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessage (AttachTreachery treacheryId (InvestigatorTarget iid))
      RexsCurse <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    Will (PassedSkillTest iid _ _ SkillTestInitiatorTarget _)
      | Just iid == treacheryAttachedInvestigator -> do
        let
          ability = (mkAbility (TreacherySource treacheryId) 0 ForcedAbility)
            { abilityLimit = PerTestOrAbility
            }
        usedAbilities <- map unUsedAbility <$> asks (getList ())
        when ((iid, ability) `notElem` usedAbilities) $ do
          withQueue $ \queue ->
            let
              (before, after) = flip break queue $ \case
                Ask iid' (ChooseOne [SkillTestApplyResults]) | iid == iid' ->
                  True
                _ -> False
              remaining = case after of
                [] -> []
                (_ : xs) -> xs
            in (before <> remaining, ())
          unshiftMessages
            [ ActivateCardAbilityAction iid ability
            , ReturnSkillTestRevealedTokens
            , AddSkillTestSubscriber (TreacheryTarget treacheryId)
            , DrawAnotherToken iid 0
            ]
        pure t
    FailedSkillTest iid _ _ (TreacheryTarget tid) _ | tid == treacheryId -> do
      t <$ unshiftMessage (ShuffleIntoDeck iid (TreacheryTarget treacheryId))
    _ -> RexsCurse <$> runMessage msg attrs
