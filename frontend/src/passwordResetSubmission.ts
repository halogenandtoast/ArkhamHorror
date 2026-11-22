export type PasswordResetSubmissionResult = 'submitted' | 'failed'

export async function submitPasswordReset(
  email: string,
  request: (email: string) => Promise<unknown>,
): Promise<PasswordResetSubmissionResult> {
  try {
    await request(email)
    return 'submitted'
  } catch {
    return 'failed'
  }
}
