{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Treachery.Cards.Chronophobia
  ( chronophobia
  , Chronophobia(..)
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype Chronophobia = Chronophobia Attrs
  deriving newtype (Show, ToJSON, FromJSON)

chronophobia :: TreacheryId -> Maybe InvestigatorId -> Chronophobia
chronophobia uuid iid = Chronophobia $ weaknessAttrs uuid iid "02039"

instance HasModifiersFor env Chronophobia where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Chronophobia where
  getActions iid NonFast (Chronophobia a@Attrs {..}) =
    withTreacheryInvestigator a $ \tormented -> do
      canActivate <- getCanAffordCost
        iid
        (toSource a)
        (ActionCost 2 Nothing treacheryTraits)
      investigatorLocationId <- getId @LocationId iid
      treacheryLocation <- getId tormented
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (TreacherySource treacheryId) 1 (ActionAbility 2 Nothing)
            )
        | treacheryLocation == investigatorLocationId && canActivate
        ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env Chronophobia where
  runMessage msg t@(Chronophobia attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ unshiftMessage (AttachTreachery treacheryId $ InvestigatorTarget iid)
    EndTurn iid | InvestigatorTarget iid `elem` treacheryAttachedTarget ->
      t <$ unshiftMessage (InvestigatorDirectDamage iid (toSource attrs) 0 1)
    UseCardAbility _ (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> Chronophobia <$> runMessage msg attrs
