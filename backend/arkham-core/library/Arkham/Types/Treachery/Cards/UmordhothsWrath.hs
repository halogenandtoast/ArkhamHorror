{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.UmordhothsWrath where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude

newtype UmordhothsWrath = UmordhothsWrath Attrs
  deriving newtype (Show, ToJSON, FromJSON)

umordhothsWrath :: TreacheryId -> a -> UmordhothsWrath
umordhothsWrath uuid _ = UmordhothsWrath $ baseAttrs uuid "01158"

instance HasModifiersFor env UmordhothsWrath where
  getModifiersFor _ _ _ = pure []

instance HasActions env UmordhothsWrath where
  getActions i window (UmordhothsWrath attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env UmordhothsWrath where
  runMessage msg t@(UmordhothsWrath attrs@Attrs {..}) = case msg of
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget n
      | tid == treacheryId -> t
      <$ unshiftMessage (HandlePointOfFailure iid (TreacheryTarget tid) n)
    HandlePointOfFailure _ (TreacheryTarget tid) 0 | tid == treacheryId ->
      pure t
    HandlePointOfFailure iid (TreacheryTarget tid) n | tid == treacheryId -> do
      cardCount' <- unCardCount <$> asks (getCount iid)
      if cardCount' > 0
        then t <$ unshiftMessages
          [ Ask iid $ ChooseOne
            [ Label "Discard a card from your hand" [RandomDiscard iid]
            , Label
              "Take 1 damage and 1 horror"
              [InvestigatorAssignDamage iid (TreacherySource treacheryId) 1 1]
            ]
          , HandlePointOfFailure iid (TreacheryTarget treacheryId) (n - 1)
          ]
        else t <$ unshiftMessages
          [ InvestigatorAssignDamage iid (TreacherySource treacheryId) 1 1
          , HandlePointOfFailure iid (TreacheryTarget treacheryId) (n - 1)
          ]
    Revelation iid source | isSource attrs source -> t <$ unshiftMessage
      (BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        5
        []
        []
        mempty
        mempty
      )
    _ -> UmordhothsWrath <$> runMessage msg attrs
