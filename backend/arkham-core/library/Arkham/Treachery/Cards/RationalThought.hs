module Arkham.Treachery.Cards.RationalThought (
  rationalThought,
  RationalThought (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Damage
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window
import Data.UUID qualified as UUID

-- The metadata makes it so that the silent forced ability triggers only once
newtype Metadata = Metadata {discarding :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RationalThought = RationalThought (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rationalThought :: TreacheryCard RationalThought
rationalThought =
  treacheryWith
    (RationalThought . (`with` Metadata False))
    Cards.rationalThought
    (tokensL %~ addTokens Horror 4)

-- NOTE: Preventing the resource gain is handled in Carolyn Fern. Ideally it
-- would be a modifier, but targetting such a specific interaction is
-- difficult. Since this is a signature, overlap like this is generally not a
-- concern, but we may want a more flexible solution in the future
instance HasModifiersFor RationalThought where
  getModifiersFor (InvestigatorTarget iid) (RationalThought (a `With` _)) =
    pure $
      toModifiers
        a
        [ CannotHealHorrorOnOtherCards (toTarget a)
        | treacheryOnInvestigator iid a
        ]
  getModifiersFor target (RationalThought a)
    | isTarget a target =
        pure $ toModifiers a [HealHorrorOnThisAsIfInvestigator "05001"] -- should this be a matcher? Probably doesn't matter as this is likely very unique
  getModifiersFor _ _ = pure []

-- Discard when no horror is on this
instance HasAbilities RationalThought where
  getAbilities (RationalThought (a `With` meta)) = case treacheryPlacement a of
    TreacheryAttachedTo _ ->
      [ mkAbility a 1 $ SilentForcedAbility AnyWindow
      | treacheryHorror a == 0 && not (discarding meta)
      ]
    _ -> []

instance RunMessage RationalThought where
  runMessage msg t@(RationalThought (attrs `With` meta)) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ AttachTreachery (toId attrs) (InvestigatorTarget iid)
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure $ RationalThought $ attrs `with` Metadata True
    HealHorror (InvestigatorTarget iid) source amount
      | unCardCode (unInvestigatorId iid)
          == UUID.toText (unTreacheryId $ toId attrs) ->
          do
            afterWindow <-
              checkWindows
                [ Window
                    Timing.After
                    ( Window.Healed
                        HorrorType
                        ( InvestigatorTarget $
                            InvestigatorId
                              ( CardCode $
                                  UUID.toText $
                                    unTreacheryId $
                                      toId
                                        attrs
                              )
                        )
                        source
                        amount
                    )
                ]
            push afterWindow
            pure
              . RationalThought
              . (`with` meta)
              $ attrs
                & tokensL
                %~ subtractTokens Horror amount
    HealHorrorDirectly (InvestigatorTarget iid) _ amount
      | unCardCode (unInvestigatorId iid)
          == UUID.toText (unTreacheryId $ toId attrs) ->
          do
            -- USE ONLY WHEN NO CALLBACKS
            pure
              . RationalThought
              . (`with` meta)
              $ attrs
                & tokensL
                %~ subtractTokens Horror amount
    _ -> RationalThought . (`with` meta) <$> runMessage msg attrs
