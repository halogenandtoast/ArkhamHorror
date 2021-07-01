module Arkham.Types.Treachery.Cards.RexsCurse
  ( RexsCurse(..)
  , rexsCurse
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype RexsCurse = RexsCurse TreacheryAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

rexsCurse :: TreacheryCard RexsCurse
rexsCurse = treachery RexsCurse Cards.rexsCurse

instance HasModifiersFor env RexsCurse where
  getModifiersFor = noModifiersFor

instance HasActions env RexsCurse where
  getActions _ _ _ = pure []

instance TreacheryRunner env => RunMessage env RexsCurse where
  runMessage msg t@(RexsCurse attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId (InvestigatorTarget iid))
    Will (PassedSkillTest iid _ _ SkillTestInitiatorTarget{} _ _)
      | treacheryOnInvestigator iid attrs -> do
        let
          ability = (mkAbility (toSource attrs) 0 ForcedAbility)
            { abilityLimit = PlayerLimit PerTestOrAbility 1
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
    FailedSkillTest iid _ _ (TreacheryTarget tid) _ _ | tid == treacheryId ->
      t <$ unshiftMessage (ShuffleIntoDeck iid (TreacheryTarget treacheryId))
    _ -> RexsCurse <$> runMessage msg attrs
